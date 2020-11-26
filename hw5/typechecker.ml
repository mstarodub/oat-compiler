open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err =
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

let builtins' = List.map (fun (x, (ts, rt)) -> x, TRef (RFun (ts, rt))) builtins

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce additional (possibly mutually recursive)
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec is_prefix_nonempt (xs' : 'a list) (ys' : 'a list) : bool =
  match xs', ys' with
  | [], _::_ -> true
  | _::_, [] | [], [] -> false
  | x::xs, y::ys -> if x <> y then false else is_prefix_nonempt xs ys

let uncurry f (a, b) = f a b
let curry f a b = f (a, b)

let all = List.fold_left (&&) true
let samelen a b = List.length a = List.length b
let zipwith f xs ys = List.map (uncurry f) (List.combine xs ys)

let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match (t1, t2) with
  | TInt, TInt
  | TBool, TBool -> true
  | TNullRef rt1, TNullRef rt2
  | TRef rt1, TRef rt2
  | TRef rt1, TNullRef rt2 -> subtype_ref c rt1 rt2
  | _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match (t1, t2) with
  | RString, RString -> true
  | RArray t1, RArray t2 -> t1 = t2
  | RStruct s1, RStruct s2
    -> begin
      let lk_s1_flist, lk_s2_flist =
        match lookup_struct_option s1 c, lookup_struct_option s2 c with
          | Some x, Some y -> (x, y)
          | None, _ | _, None -> ([], [])
      in
        (* XXX: maybe order doesnt matter. then change this to use lookup_field_option *)
        is_prefix_nonempt lk_s2_flist lk_s1_flist
    end
  | RFun (ts1, rt1), RFun (ts2, rt2)
    -> subtype_ret c rt1 rt2
      && samelen ts1 ts2
      && all @@ zipwith (subtype c) ts1 ts2
  | _ -> false

and subtype_ret (c : Tctxt.t) (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =
  match (t1, t2) with
  | RetVoid, RetVoid -> true
  | RetVal ty1, RetVal ty2 -> subtype c ty1 ty2
  | _ -> false


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the
      type is not well-formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt | TBool -> ()
  | TRef rt | TNullRef rt -> typecheck_ref l tc rt

and typecheck_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
  | RString -> ()
  | RArray ty -> typecheck_ty l tc ty
  | RStruct s
    -> if lookup_struct_option s tc <> None
      then ()
      else raise (type_error l "not a wellformed type")
  | RFun (ts, ret)
    -> typecheck_ret l tc ret;
      List.iter (typecheck_ty l tc) ts

and typecheck_ret (l : 'a Ast.node) (tc : Tctxt.t) (t: Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   three typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  match e.elt with
  | CNull rty
    -> typecheck_ty e c (TRef rty);
      TNullRef rty
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr _ -> TRef RString
  | Id i
    -> begin match lookup_local_option i c with
      | None
        -> begin match lookup_global_option i c with
          | None -> raise (type_error e "not well-typed: typ_local/typ_global")
          | Some y -> y
        end
      | Some x -> x
    end
  | CArr (ty, es)
    -> typecheck_ty e c ty;
      let ts = List.map (typecheck_exp c) es in
      if not @@ all @@ List.map (fun t -> subtype c t ty) ts
      then raise (type_error e "not well-typed: typ_carr");
      TRef (RArray ty)
  (* XXX: unsure *)
  | NewArr (ty, e1, i, e2)
    -> typecheck_ty e c ty;
    let crct = typecheck_exp c e1 = TInt
      && lookup_local_option i c = None
      && lookup_global_option i c = Some TInt (* about this one specifically *)
      && subtype c (typecheck_exp c e2) ty
    in if crct
    then TRef (RArray ty)
    else raise (type_error e "not well-typed: typ_newarray")
  | Index (e1, e2)
    -> let t = begin match typecheck_exp c e1 with
        | TRef (RArray ty) -> ty
        | _ -> raise (type_error e "not well-typed: typ_index")
      end in
      if typecheck_exp c e2 = TInt
      then t
      else raise (type_error e "not well-typed: typ_index")
  | Length e'
    -> begin match typecheck_exp c e' with
        | TRef (RArray _) -> TInt
        | _ -> raise (type_error e "not well-typed: typ_lenght")
      end
  | CStruct (i, ies)
    -> let rec aux ascl_a =
        match ascl_a with
        | [] -> true
        | (ai, ae)::axs
          -> let t = begin match lookup_field_option i ai c with
              | None -> raise (type_error e "not well-typed: typ_structex")
              | Some x -> x
            end in
            let t' = typecheck_exp c ae in
            if not (subtype c t t')
            then false
            else aux axs
      in if not @@ (lookup_struct_option i c <> None)
        && samelen ies (lookup_struct i c)
        && aux ies
      then raise (type_error e "not well-typed: typ_structex");
      TRef (RStruct i)
  | Proj (e', i)
    -> let str = begin match typecheck_exp c e' with
        | TRef (RStruct i) when List.mem i (List.map fst c.structs) -> i
        | _ -> raise (type_error e "not well-typed: typ_field")
      end
      in let field_ty = match lookup_field_option str i c with
        | None -> raise (type_error e "not well-typed: typ_field")
        | Some x -> x
      in field_ty
  | Call (f, es)
    -> let f_ty = match typecheck_exp c f with
        | TRef (RFun (arg_tys, r_ty)) -> (arg_tys, r_ty)
        | _ -> raise (type_error e "not well-typed: typ_call")
      in let tys = List.map (typecheck_exp c) es in
      if all @@ zipwith (subtype c) tys (fst f_ty)
      (* XXX: so far all refty's wrapped in TRef. but here things get weird. what if we have RetVoid ? if we would return a sum type, could write snd f_ty here... *)
      then
        begin match snd f_ty with
          | RetVoid -> failwith "type of void ???"
          | RetVal ty -> ty
        end
      else raise (type_error e "not well-typed: typ_call")
  | Bop (bop, e1, e2)
    -> let t1 = typecheck_exp c e1 in
      let t2 = typecheck_exp c e2 in
      if bop = Eq || bop = Neq
      then
        if subtype c t1 t2 && subtype c t2 t1
        then TBool
        else raise (type_error e "not well-typed: typ_eq")
      else
        let (tbop1, tbop2, tbop3) = typ_of_binop bop in
        if tbop1 = t1 && tbop2 = t2
        then tbop3
        else raise (type_error e "not well-typed: typ_bop")
  | Uop (uop, e')
    -> let tuop = fst @@ typ_of_unop uop in
      if typecheck_exp c e' = tuop
      then tuop
      else raise (type_error e "not well-typed: typ_uop")

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement
   This function should implement the statment typechecking rules from oat.pdf.

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement
     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns

        in the branching statements, both branches must definitely return

        Intuitively: if one of the two branches of a conditional does not
        contain a return statement, then the entier conditional statement might
        not return.

        looping constructs never definitely return

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  failwith "todo: implement typecheck_stmt"


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id)
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration
    - extends the local context with the types of the formal parameters to the
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  failwith "todo: typecheck_fdecl"

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields)

     H_1 |-s prog ==> H_2


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G_1 |-f prog ==> G_2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G_1 |-g prog ==> G_2


   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

let rec has_duplicate (l : 'a list) : bool =
  match l with
  | [] -> false
  | x::xs -> (List.mem x xs) || has_duplicate xs

let rec filter_prog_tdecl (p:Ast.prog) : Ast.tdecl node list =
  match p with
  | [] -> []
  | d::ds
    -> begin match d with
      | Gtdecl x
        -> if has_duplicate (snd x.elt)
          then raise (type_error x "not well-typed: duplicate struct field")
          else [x]
      | _ -> []
    end @ filter_prog_tdecl ds

(* XXX: typ_sgdecl, typ_sfdecl ? *)
let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  let addstructs ctxt (newst : Ast.tdecl node) : Tctxt.t =
    let id = fst newst.elt in
    let fs = snd newst.elt in
    match lookup_struct_option id ctxt with
      | None -> add_struct ctxt id fs
      | Some _ -> raise (type_error newst "not well-typed: duplicate struct def.")
  in List.fold_left addstructs Tctxt.empty (filter_prog_tdecl p)

let rec filter_prog_fdecl (p:Ast.prog) : Ast.fdecl node list =
  match p with
  | [] -> []
  | d::ds
    -> begin match d with
      | Gfdecl f -> [f]
      | _ -> []
    end @ filter_prog_fdecl ds

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let addfuncs ctxt (newfun : Ast.fdecl node) : Tctxt.t =
    let id = newfun.elt.fname in
    let ty = TRef (RFun (List.map fst newfun.elt.args, newfun.elt.frtyp)) in
    match lookup_global_option id ctxt with
      | None -> add_global ctxt id ty
      | Some _ -> raise (type_error newfun "not well-typed: function already def.")
  in List.fold_left
    addfuncs
    (List.fold_left (fun c -> uncurry (add_global c)) tc builtins')
    (filter_prog_fdecl p)

let rec filter_prog_gdecl (p:Ast.prog) : Ast.gdecl node list =
  match p with
  | [] -> []
  | d::ds
    -> begin match d with
      | Gvdecl g -> [g]
      | _ -> []
    end @ filter_prog_gdecl ds

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let addglobals ctxt (newglob : Ast.gdecl node) : Tctxt.t =
    let id = newglob.elt.name in
    let ex = newglob.elt.init in
    match lookup_global_option id ctxt with
      (* tc in inner typecheck, because we cannot reference other globals *)
      | None -> add_global ctxt id (typecheck_exp tc ex)
      | Some _ -> raise (type_error newglob "not well-typed: global already def.")
  in List.fold_left addglobals tc (filter_prog_gdecl p)

(* This function implements the |- prog and the H ; G |- prog
   rules of the oat.pdf specification.
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l
    | _ -> ()) p
