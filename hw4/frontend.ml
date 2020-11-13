open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for
     compiling local variable declarations
*)

type elt =
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))
let lift_entry : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> E (x,i))
let lift_global : (gid * Ll.gdecl) list -> stream = List.rev_map (fun (x,i) -> G (x, i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None ->
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some term ->
              (gs, einsns, [], None, (l, {insns; term})::blks)
           end
        | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator"
    | Some term ->
       let insns = einsns @ insns in
       ({insns; term}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None

end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) ->
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* Compiler Invariants

   The LLVM IR type of a variable (whether global or local) that stores an Oat
   array value (or any other reference type, like "string") will always be a
   double pointer.  In general, any Oat variable of Oat-type t will be
   represented by an LLVM IR value of type Ptr (cmp_ty t).  So the Oat variable
   x : int will be represented by an LLVM IR value of type i64*, y : string will
   be represented by a value of type i8**, and arr : int[] will be represented
   by a value of type {i64, [0 x i64]}**.  Whether the LLVM IR type is a
   "single" or "double" pointer depends on whether t is a reference type.

   We can think of the compiler as paying careful attention to whether a piece
   of Oat syntax denotes the "value" of an expression or a pointer to the
   "storage space associated with it".  This is the distinction between an
   "expression" and the "left-hand-side" of an assignment statement.  Compiling
   an Oat variable identifier as an expression ("value") does the load, so
   cmp_exp called on an Oat variable of type t returns (code that) generates a
   LLVM IR value of type cmp_ty t.  Compiling an identifier as a left-hand-side
   does not do the load, so cmp_lhs called on an Oat variable of type t returns
   and operand of type (cmp_ty t)*.  Extending these invariants to account for
   array accesses: the assignment e1[e2] = e3; treats e1[e2] as a
   left-hand-side, so we compile it as follows: compile e1 as an expression to
   obtain an array value (which is of pointer of type {i64, [0 x s]}* ).
   compile e2 as an expression to obtain an operand of type i64, generate code
   that uses getelementptr to compute the offset from the array value, which is
   a pointer to the "storage space associated with e1[e2]".

   On the other hand, compiling e1[e2] as an expression (to obtain the value of
   the array), we can simply compile e1[e2] as a left-hand-side and then do the
   load.  So cmp_exp and cmp_lhs are mutually recursive.  [[Actually, as I am
   writing this, I think it could make sense to factor the Oat grammar in this
   way, which would make things clearer, I may do that for next time around.]]


   Consider globals7.oat

   /--------------- globals7.oat ------------------
   global arr = int[] null;

   int foo() {
     var x = new int[3];
     arr = x;
     x[2] = 3;
     return arr[2];
   }
   /------------------------------------------------

   The translation (given by cmp_ty) of the type int[] is {i64, [0 x i64] }* so
   the corresponding LLVM IR declaration will look like:

   @arr = global { i64, [0 x i64] }* null

   This means that the type of the LLVM IR identifier @arr is {i64, [0 x i64] }**
   which is consistent with the type of a locally-declared array variable.

   The local variable x would be allocated and initialized by (something like)
   the following code snippet.  Here %_x7 is the LLVM IR uid containing the
   pointer to the "storage space" for the Oat variable x.

   %_x7 = alloca { i64, [0 x i64] }*                              ;; (1)
   %_raw_array5 = call i64*  @oat_alloc_array(i64 3)              ;; (2)
   %_array6 = bitcast i64* %_raw_array5 to { i64, [0 x i64] }*    ;; (3)
   store { i64, [0 x i64]}* %_array6, { i64, [0 x i64] }** %_x7   ;; (4)

   (1) note that alloca uses cmp_ty (int[]) to find the type, so %_x7 has
       the same type as @arr

   (2) @oat_alloc_array allocates len+1 i64's

   (3) we have to bitcast the result of @oat_alloc_array so we can store it
        in %_x7

   (4) stores the resulting array value (itself a pointer) into %_x7

  The assignment arr = x; gets compiled to (something like):

  %_x8 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7     ;; (5)
  store {i64, [0 x i64] }* %_x8, { i64, [0 x i64] }** @arr       ;; (6)

  (5) load the array value (a pointer) that is stored in the address pointed
      to by %_x7

  (6) store the array value (a pointer) into @arr

  The assignment x[2] = 3; gets compiled to (something like):

  %_x9 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7      ;; (7)
  %_index_ptr11 = getelementptr { i64, [0 x  i64] },
                  { i64, [0 x i64] }* %_x9, i32 0, i32 1, i32 2   ;; (8)
  store i64 3, i64* %_index_ptr11                                 ;; (9)

  (7) as above, load the array value that is stored %_x7

  (8) calculate the offset from the array using GEP

  (9) store 3 into the array

  Finally, return arr[2]; gets compiled to (something like) the following.
  Note that the way arr is treated is identical to x.  (Once we set up the
  translation, there is no difference between Oat globals and locals, except
  how their storage space is initially allocated.)

  %_arr12 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** @arr    ;; (10)
  %_index_ptr14 = getelementptr { i64, [0 x i64] },
                 { i64, [0 x i64] }* %_arr12, i32 0, i32 1, i32 2  ;; (11)
  %_index15 = load i64, i64* %_index_ptr14                         ;; (12)
  ret i64 %_index15

  (10) just like for %_x9, load the array value that is stored in @arr

  (11)  calculate the array index offset

  (12) load the array value at the index

*)

(* Global initialized arrays:

  There is another wrinkle: To compile global initialized arrays like in the
  globals4.oat, it is helpful to do a bitcast once at the global scope to
  convert the "precise type" required by the LLVM initializer to the actual
  translation type (which sets the array length to 0).  So for globals4.oat,
  the arr global would compile to (something like):

  @arr = global { i64, [0 x i64] }* bitcast
           ({ i64, [4 x i64] }* @_global_arr5 to { i64, [0 x i64] }* )
  @_global_arr5 = global { i64, [4 x i64] }
                  { i64 4, [4 x i64] [ i64 1, i64 2, i64 3, i64 4 ] }

*)



(* Some useful helper functions *)

let rec zip3 (a:'a list) (b: 'b list) (c: 'c list) : ('a * 'b * 'c) list =
  begin match a with
    | [] -> []
    | (x::xs) -> begin match b with
      | [] -> []
      | (y::ys) -> begin match c with
        | [] -> []
        | (z::zs) -> (x, y, z) :: (zip3 xs ys zs)
      end
    end
  end

let rec split3 (l: ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  match l with
    | [] -> ([], [], [])
    | (x, y, z)::tl -> 
      let (xs, ys, zs) = split3 tl in
      (x::xs, y::ys, z::zs)

let int64_of_bool (b: bool) : int64 = if b then 1L else 0L
let deref (ptr:Ll.ty) : Ll.ty =
  match ptr with
    | Ptr x -> x
    | _ -> failwith "tried to deref non-pointer type"

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the stack, in bytes.
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate a zero-initialized array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = Ptr I64 in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]

(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression.

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to make sure
     either that the resulting gid has type (Ptr I8), or, if the gid has type
     [n x i8] (where n is the length of the string), convert the gid to a
     (Ptr I8), e.g., by using getelementptr.

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

*)
let cmp_gstr (s:string) : Ll.ginit * (Ll.gid * Ll.gdecl) list =
  let str_sym = gensym "gstr" in
  (* Add 1 to length of string because of null terminator *)
  let str_ty = Array (1 + (String.length s), I8) in
  let str_gdecl = (str_ty, GString s) in
  let str_predecl = [(str_sym, str_gdecl)] in
  (GBitcast (Ptr str_ty, GGid str_sym, Ptr I8), str_predecl)

let cmp_str_exp (s:string) : Ll.ty * Ll.operand * stream =
  let ty = cmp_ty (TRef RString) in

  let str_gid = gensym "exp_str" in
  let (ginit, predecls) = cmp_gstr s in
  let gdecl = (ty, ginit) in
  let all_gdecls = (str_gid, gdecl) :: predecls in
  let g_stream = lift_global all_gdecls in

  let dest_uid = gensym "exp_str" in
  let load = I (dest_uid, Ll.Load (Ptr ty, Gid str_gid)) in

  let stream = load :: g_stream in
  (ty, Id dest_uid, stream)

let rec cmp_exp (c:Ctxt.t) ({elt=exp; _}:Ast.exp node) : Ll.ty * Ll.operand * stream =
  match exp with
    | CNull rty -> (cmp_ty (TRef rty), Null, [])
    | CBool b -> (cmp_ty TBool, Const (int64_of_bool b), [])
    | CInt i -> (cmp_ty TInt, Const i, [])

    | CStr s -> cmp_str_exp s
    
    | CArr (ty, exprs) ->
      (* allocate array *)
      let size = List.length exprs in
      let (arr_ty, arr_op, arr_stream) = oat_alloc_array ty (Const (Int64.of_int size)) in
      
      (* compile expressions *)
      let cmp_exprs = List.map (cmp_exp c) exprs in
      let (expr_tys, expr_ops, expr_streams) = split3 cmp_exprs in

      (* move expressions into corresponding array slots *)
      let gep_of_index i = Gep (arr_ty, arr_op, [Const 0L; Const 1L; Const (Int64.of_int i)]) in
      let ptr_uids = List.init size (fun i -> gensym "exp_carr") in
      let geps = List.init size gep_of_index in

      let named_geps = List.combine ptr_uids geps in
      let gep_insns = List.map (fun (ptr_uid, gep) -> I (ptr_uid, gep)) named_geps in

      let store_args = zip3 expr_tys expr_ops ptr_uids in
      let stores = List.map (fun (ty, op, dest_uid) -> Store (ty, op, Id dest_uid)) store_args in
      let store_insns = List.map (fun i -> I (gensym "store", i)) stores in

      let new_stream = store_insns @ gep_insns @ (List.flatten expr_streams) @ arr_stream in
      (arr_ty, arr_op, new_stream)

    | NewArr (ty, exp) -> 
      let (exp_ty, exp_op, exp_stream) = cmp_exp c exp in
      let (arr_ty, arr_op, arr_stream) = oat_alloc_array ty exp_op in
      (arr_ty, arr_op, arr_stream @ exp_stream)

    | Id id -> let (ll_ty, ll_op) = Ctxt.lookup id c in
      let dest_uid = gensym "exp_id" in
      (deref ll_ty, Id dest_uid, [I (dest_uid, Ll.Load (ll_ty, ll_op))])
    
    | Index (exp1, exp2) -> cmp_index c exp1 exp2

    | Call (exp, exprs) -> failwith "cmp_exp unimplemented Call"

    | Bop (bop, exp1, exp2) -> cmp_binop c bop exp1 exp2
    | Uop (uop, exp) -> cmp_uop c uop exp

and cmp_index (c:Ctxt.t) (exp1:Ast.exp node) (exp2:Ast.exp node) : Ll.ty * Ll.operand * stream =
  let (ptr_ty, ptr_op, ptr_stream) = cmp_lhs c (no_loc (Index (exp1, exp2))) in

  let val_uid = gensym "exp_index" in
  let load = Load (ptr_ty, ptr_op) in
  let load_insn = I (val_uid, load) in

  let new_stream = load_insn :: (ptr_stream) in
  (deref ptr_ty, Id val_uid, new_stream)

and cmp_uop (c:Ctxt.t) (uop:unop) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  let (ll_ty, ll_op, stream) = cmp_exp c exp in
  let dest_uid = gensym "exp_uop" in
  let dest_ty = (if uop = Lognot then TBool else TInt) |> cmp_ty in
  let insn = match uop with
    | Neg -> Ll.Binop (Sub, dest_ty, Const 0L, ll_op)
    | Lognot -> Ll.Binop (Xor, dest_ty, Const 1L, ll_op)
    | Bitnot -> Ll.Binop (Xor, dest_ty, Const (-1L), ll_op)
  in
  (dest_ty, Id dest_uid, (I (dest_uid, insn)) :: stream)

and cmp_binop (c:Ctxt.t) (bop:binop) (exp1:Ast.exp node) (exp2:Ast.exp node) : Ll.ty * Ll.operand * stream =
  let (ll_ty_1, ll_op_1, stream_1) = cmp_exp c exp1 in
  let (ll_ty_2, ll_op_2, stream_2) = cmp_exp c exp2 in
  let dest_uid = gensym "exp_binop" in
  let dest_ty = match bop with
    | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.And | Ast.Or -> cmp_ty TBool
    | Ast.Add | Ast.Sub | Ast.Mul | Ast.IAnd | Ast.IOr | Ast.Shl | Ast.Shr | Ast.Sar -> cmp_ty TInt
  in
  let insn = match bop with
    | Add -> Ll.Binop (Add, ll_ty_1, ll_op_1, ll_op_2)
    | Sub -> Ll.Binop (Sub, ll_ty_1, ll_op_1, ll_op_2)
    | Mul -> Ll.Binop (Mul, ll_ty_1, ll_op_1, ll_op_2)
    | IAnd -> Ll.Binop (And, ll_ty_1, ll_op_1, ll_op_2)
    | IOr -> Ll.Binop (Or, ll_ty_1, ll_op_1, ll_op_2)
    | Shl -> Ll.Binop (Shl, ll_ty_1, ll_op_1, ll_op_2)
    | Shr -> Ll.Binop (Lshr, ll_ty_1, ll_op_1, ll_op_2)
    | Sar -> Ll.Binop (Ashr, ll_ty_1, ll_op_1, ll_op_2)

    | Eq -> Ll.Icmp (Eq, ll_ty_1, ll_op_1, ll_op_2)
    | Neq -> Ll.Icmp (Ne, ll_ty_1, ll_op_1, ll_op_2)
    | Lt -> Ll.Icmp (Slt, ll_ty_1, ll_op_1, ll_op_2)
    | Lte -> Ll.Icmp (Sle, ll_ty_1, ll_op_1, ll_op_2)
    | Gt -> Ll.Icmp (Sgt, ll_ty_1, ll_op_1, ll_op_2)
    | Gte -> Ll.Icmp (Sge, ll_ty_1, ll_op_1, ll_op_2)

    | And -> Ll.Binop (And, ll_ty_1, ll_op_1, ll_op_2)
    | Or -> Ll.Binop (Or, ll_ty_1, ll_op_1, ll_op_2)
  in
  (dest_ty, Id dest_uid, (I (dest_uid, insn)) :: stream_1 @ stream_2)

and cmp_lhs (c:Ctxt.t) ({elt=exp; _}:Ast.exp node) : Ll.ty * Ll.operand * stream =
  match exp with
    | Id id ->
      let (ty, op) = Ctxt.lookup id c in
      (ty, op, [])

    | Index (exp1, exp2) ->
      let (ll_ty_1, ll_op_1, stream_1) = cmp_exp c exp1 in
      let (ll_ty_2, ll_op_2, stream_2) = cmp_exp c exp2 in

      let ptr_uid = gensym "exp_index" in
      let el_ty = match ll_ty_1 with
        | Ptr (Struct [I64; Array (0, ty)]) -> ty
        (* TODO: Can remove this eventually, is useful for testing *)
        | _ -> failwith (String.concat " " ["Cannot index to non-array type"; Llutil.string_of_ty ll_ty_1; ""])
      in
      (* gep ty op 0 1 index *)
      let gep = Gep (ll_ty_1, ll_op_1, [Const 0L; Const 1L; ll_op_2]) in
      let gep_insn = I (ptr_uid, gep) in

      let new_stream = gep_insn :: (stream_1 @ stream_2) in
      (Ptr el_ty, Id ptr_uid, new_stream)
    
    | _ -> failwith "invalid lhs expression"

    
(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable
     declarations

   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)

let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) ({elt=stmt; _}:Ast.stmt node) : Ctxt.t * stream =
  match stmt with
    | Ret None -> (c, [T (Ret (rt, None))])
    | Ret (Some exp) ->
        let (ty, op, stream) = cmp_exp c exp in
        (* TODO: probably can remove this because typechecking isn't required *)
        (* It's useful for testing though *)
        if ty <> rt then failwith (String.concat " " ["Type mismatch between"; Llutil.string_of_ty ty; "and"; Llutil.string_of_ty rt; ""]);
        let ret = T (Ret (rt, Some op)) in
        (c, ret :: stream)

    | Decl (id, exp) ->
        let (ty, op, stream) = cmp_exp c exp in
        let dest_uid = gensym "stmt_decl" in

        let alloca = E (dest_uid, Alloca ty) in
        let store = I (gensym "store", Store (ty, op, Id dest_uid)) in

        let new_stream = alloca :: store :: stream in
        let new_ctxt = Ctxt.add c id (Ptr ty, Id dest_uid) in

        (new_ctxt, new_stream)
    
    | Assn (exp1, exp2) ->
      let (lhs_ty, lhs_op, lhs_stream) = cmp_lhs c exp1 in
      let (rhs_ty, rhs_op, rhs_stream) = cmp_exp c exp2 in
      (* TODO: probably can remove this because typechecking isn't required *)
      (* It's useful for testing though *)
      if lhs_ty <> Ptr (rhs_ty) then failwith (String.concat " " ["Cannot assign, type mismatch between"; Llutil.string_of_ty lhs_ty; "and"; Llutil.string_of_ty rhs_ty; ""]);
        
      let store = I (gensym "store", Store (rhs_ty, rhs_op, lhs_op)) in
      let new_stream = store :: (lhs_stream @ rhs_stream) in

      (c, new_stream)

    | SCall (exp, exprs) -> failwith "cmp_stmt unimplemented SCall"
    | If (exp, stmts1, stmts2) -> failwith "cmp_stmt unimplemented If"
    | For (vdecls, opt_exp, opt_stmt, stmts) -> failwith "cmp_stmt unimplemented For"
    | While (exp, stmts) -> failwith "cmp_stmt unimplemented While"
  

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : Ctxt.t * stream =
  List.fold_left (fun (c, code) s ->
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts



(* Adds each function identifer to the context at an
   appropriately translated type.

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
         let ft = TRef (RFun (List.map fst args, frtyp)) in
         Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p

(* Populate a context with bindings for global variables
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C).
*)
let resolve_gexp_type exp =
  let ty = match exp with
  | CNull rty -> TRef rty
  | CBool b -> TBool
  | CInt i -> TInt
  | CStr s -> TRef (RString)
  | CArr (ty, _) -> TRef (RArray ty)
  | _ -> failwith "invalid global initializer expression"
  in cmp_ty ty

let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  let f t decl = match decl with
    | Gfdecl _ -> t
    (* name = exp *)
    | Gvdecl { elt={ name; init={ elt=exp; _ } }; _ } ->
      Ctxt.add t name (Ptr (resolve_gexp_type exp), Gid name)
  in
  List.fold_left f Ctxt.empty p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   4. Compile the body of the function using cmp_block
   5. Use cfg_of_stream to produce a LLVMlite cfg from
 *)

let cmp_fdecl (c:Ctxt.t) ({elt=f; _}:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let { frtyp; fname; args; body } = f in
  let (arg_tys, arg_ids) = List.split args in

  (* Argument values *)
  let arg_ll_tys = List.map cmp_ty arg_tys in
  let arg_uids_src = List.map (fun s -> gensym @@ String.concat "_" [fname; s]) arg_ids in
  let arg_ops_src = List.map (fun uid -> Ll.Id uid) arg_uids_src in

  (* Argument pointers *)
  let arg_ll_ptr_tys = List.map (fun ty -> Ptr ty) arg_ll_tys in
  let arg_uids_dest = List.map (fun s -> gensym @@ String.concat "_" [fname; s]) arg_ids in
  let arg_ops_dest = List.map (fun uid -> Ll.Id uid) arg_uids_dest in

  (* LL function parameters and type signature *)
  let ll_f_param = arg_uids_src in
  let ll_rty = cmp_ret_ty frtyp in
  let ll_f_ty = (arg_ll_tys, ll_rty) in

  (* Allocas *)
  let allocas = List.map (fun ll_ty -> Alloca ll_ty) arg_ll_tys in
  let alloca_insns = List.combine arg_uids_dest allocas in
  
  (* Stores *)
  let store_args = zip3 arg_ll_tys arg_ops_src arg_ops_dest in
  let stores = List.map (fun (ll_ty, src, dest) -> Store (ll_ty, src, dest)) store_args in
  let store_syms = List.init (List.length stores) (fun i -> gensym "store") in
  let store_insns = List.combine store_syms stores in

  (* Update context *)
  let bindings = List.combine arg_ids @@ List.combine arg_ll_ptr_tys arg_ops_dest in
  let start_ctxt = List.fold_left (fun c (id, bnd) -> Ctxt.add c id bnd) c bindings in

  (* Compile function body *)
  let setup_stream = lift_entry (alloca_insns @ store_insns) in
  let (end_ctxt, stream) = cmp_block start_ctxt ll_rty body in

  (* Create CFG *)
  let (ll_cfg, gdecls) = cfg_of_stream (stream @ setup_stream) in

  ({ f_ty=ll_f_ty; f_param=ll_f_param; f_cfg=ll_cfg }, gdecls)

(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations.
*)

let rec cmp_gexp (c:Ctxt.t) ({elt=exp; _}:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  let ll_ty = resolve_gexp_type exp in
  let (ginit, predecls) = match exp with
    | CNull rty -> (GNull, [])
    | CBool b -> (GInt (int64_of_bool b), [])
    | CInt i -> (GInt i, [])
    | CStr s -> cmp_gstr s
    | CArr (ty, exprs) -> cmp_garr c ty exprs
    | _ -> failwith "invalid global expression"
  in
  let gdecl = (ll_ty, ginit) in
  (gdecl, predecls)

and cmp_garr (c:Ctxt.t) (ty:Ast.ty) (exprs:Ast.exp node list) : Ll.ginit * (Ll.gid * Ll.gdecl) list =
  let arr_len = List.length exprs in
  let arr_sym = gensym "garr" in
  let arr_ty = Struct [I64; Array (arr_len, cmp_ty ty)] in
  let final_ty = Struct [I64; Array (0, cmp_ty ty)] in
  match ty with
    | TRef r -> cmp_garr_ref c r exprs
    | _ ->
      let gdecls = exprs |> List.map (cmp_gexp c) |> List.map fst in
      let arr_gdecl = (arr_ty, GStruct [(I64, GInt (Int64.of_int arr_len)); (Array (arr_len, cmp_ty ty), GArray gdecls)]) in
      let arr_predecl = [(arr_sym, arr_gdecl)] in
      (GBitcast (Ptr arr_ty, GGid arr_sym, Ptr final_ty), arr_predecl)

and cmp_garr_ref (c:Ctxt.t) (r:Ast.rty) (exprs:Ast.exp node list) : Ll.ginit * (Ll.gid * Ll.gdecl) list =
  let r_ty = cmp_rty r in
  let el_ty = Ptr r_ty in
  let arr_len = List.length exprs in
  let arr_sym = gensym "garr" in
  let arr_ty = Struct [I64; Array (arr_len, Ptr el_ty)] in
  let final_ty = Struct [I64; Array (0, el_ty)] in

  let expr_syms = List.init arr_len (fun i -> gensym "garr_el") in
  let (expr_gdecls, expr_predecls) = exprs |> List.map (cmp_gexp c) |> List.split in
  let expr_decls = List.combine expr_syms expr_gdecls in

  let expr_gids = List.map (fun i -> (Ptr el_ty, GGid i)) expr_syms in
  let arr_gdecl = (arr_ty, GStruct [(I64, GInt (Int64.of_int arr_len)); (Array (arr_len, Ptr el_ty), GArray expr_gids)]) in
  let arr_predecl = (arr_sym, arr_gdecl) in

  let all_predecls = arr_predecl :: expr_decls @ (List.flatten expr_predecls) in
  (GBitcast (Ptr arr_ty, GGid arr_sym, Ptr final_ty), all_predecls)

(* Oat internals function context ------------------------------------------- *)
let internals = [
    "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt =
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls =
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } ->
           let ll_gd, gs' = cmp_gexp c gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }
