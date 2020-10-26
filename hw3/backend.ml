(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understanding this entire file and
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page.
*)


(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge



(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            ; frame_size : int
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m


(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip).

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).
*)
let interm_mov (o:operand) (dest:operand) = [
  (Movq, [o; Reg R10]);
  (Movq, [Reg R10; dest])
]

let compile_operand (ctxt:ctxt) (dest:X86.operand) : Ll.operand -> ins list =
  let { tdecls; layout; frame_size } = ctxt in
  let open Asm in
  function ll_op -> match ll_op with
    | Null -> [Movq, [~$0; dest]]
    | Const i -> [Movq, [Imm (Lit i); dest]]
    | Gid g -> [Leaq, [Ind3 ((Lbl (Platform.mangle g)), Rip); dest]]
    | Id u -> interm_mov (lookup layout u) dest



(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)
let arg_loc_call (n : int) : operand =
  match n with
    | 0 -> Reg Rdi
    | 1 -> Reg Rsi
    | 2 -> Reg Rdx
    | 3 -> Reg Rcx
    | 4 -> Reg R08
    | 5 -> Reg R09
    | i -> let displacement = ((i - 6)) * 8 in Ind3(Lit (Int64.of_int displacement), Rsp)

let rec interleave (a:'a list) (b:'a list) : 'a list =
  match a with
    | [] -> b
    | x::xs -> x::(interleave b xs)

let compile_call (ctxt:ctxt) (ty:Ll.ty) (op:Ll.operand) (argl:(Ll.ty * Ll.operand) list) : ins list =
  (* Move arguments into registers and stack slots *)
  let operands = List.map snd argl in
  let intermediates = List.map (compile_operand ctxt (Reg R10)) operands in
  let arg_locs = List.init (List.length operands) arg_loc_call in
  let move_to_arg_locs = List.map (fun n -> [Movq, [Reg R10; n]]) arg_locs in

  (* Increment rsp by amount of arguments pushed onto stack *)
  let rsp_shift_amount = if List.length argl > 6 then 8 * (List.length argl - 6) else 0 in

  let open Asm in
  let rsp_shift = [Subq, [~$rsp_shift_amount; Reg Rsp]] in
  let rsp_unshift = [Addq, [~$rsp_shift_amount; Reg Rsp]] in
  let arg_moves = interleave intermediates move_to_arg_locs |> List.flatten in
  let call = compile_operand ctxt (Reg Rax) op @ [(Callq, [Reg Rax])] in

  rsp_shift @ arg_moves @ call @ rsp_unshift

(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelementptr, you must generate x86 code that performs
   the appropriate arithmetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes.
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let sum l = List.fold_left (+) 0 l

let rec size_ty (tdecls:(tid * ty) list) (t:Ll.ty) : int =
  match t with
    | Void | I8 | Fun _ -> 0
    | I1 | I64 | Ptr _ -> 8
    | Struct tlist -> sum (List.map (size_ty tdecls) tlist)
    | Array (l, t) -> l * (size_ty tdecls t)
    | Namedt tid -> size_ty tdecls (lookup tdecls tid)


(* Generates code that computes a pointer value.

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

     - if t is a struct, the index must be a constant n and it
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the
       sizes of the types of the previous elements ]

     - if t is an array, the index can be any operand, and its
       value determines the offset within the array.

     - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)
let rec take (n: int) (l:'a list) : 'a list =
  match (n, l) with
    | (n, _) when n <= 0 -> []
    | (_, []) -> []
    | (n, x::xs) -> x :: (take (n-1) xs)

let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
  let open Asm in
  let { tdecls; layout; frame_size } = ctxt in

  let rec next_array_addr t i is : ins list =
    (* compute offset based on index and type size *)
    let size_t = size_ty tdecls t in
    let load_index = compile_operand ctxt ~%Rcx i in
    let compute_offset = [Imulq, [~$size_t; ~%Rcx]] in
    (* add offset to current address *)
    let next_addr = [Addq, [~%Rcx; ~%Rax]] in
    load_index @ compute_offset @ next_addr @ (decide t is)

  and next_struct_addr tys i is : ins list =
    (* get index from const value *)
    let index = match i with
      | Const c -> Int64.to_int c
      | _ -> failwith "GEP struct index must be const"
    in
    let next_ty = List.nth tys index in
    (* compute offset based on type sizes *)
    let ty_sizes = List.map (size_ty tdecls) tys in
    let offset = take index ty_sizes |> sum in
    (* add offset to current addr *)
    let next_addr = [Addq, [~$offset; ~%Rax]] in
    next_addr @ (decide next_ty is)

  and decide t is : ins list =
    (* Current address is loaded in Rax *)
    (* Current type is t *)
    (* Return address should be in Rax *)
    match is with
      | [] -> []
      | i::is -> begin match t with
        | Struct tys -> next_struct_addr tys i is
        | Array (l, ty) -> next_array_addr ty i is
        (* resolve named types *)
        | Namedt tid -> decide (lookup tdecls tid) (i::is)
        | _ -> failwith "invalid GEP path"
      end
  in

  let start t op =
    let load_addr = compile_operand ctxt ~%Rax op in
    match path with
      (* path should not be empty, must contain at least one operand *)
      | [] -> failwith "empty path in GEP"
      (* first operand is always considered as array index *)
      | (i::is) -> load_addr @ (next_array_addr t i is)
  in
  match op with
    | (Ptr t, op) -> start t op
    | _ -> failwith "GEP only defined for Ptr values"

let compile_bop (ctxt:ctxt) : Ll.bop -> ins list =
(* first arg in rax, second arg in rcx *)
  let open Asm in
  function ll_bop -> let i = match ll_bop with
    | Add -> Addq
    | Sub -> Subq
    | Mul -> Imulq
    | Shl -> Shlq
    | Lshr -> Shrq
    | Ashr -> Sarq
    | And -> Andq
    | Or -> Orq
    | Xor -> Xorq
    in [i, [~%Rcx; ~%Rax]]

let compile_icmp ({ tdecls; layout; frame_size }:ctxt) (cnd:Ll.cnd) (ty:Ll.ty) =
  let open Asm in
  match ty with
    | I1 | I8 | I64 | Ptr _
      -> let x86_cnd = compile_cnd cnd in
                  [ Cmpq, [~%Rcx; ~%Rax]
                  (* Clear rax value (setb only sets lower byte) *)
                  ; Movq, [~$0; ~%Rax]
                  ; Set x86_cnd, [~%Rax]
                  ]
    | _ -> failwith "invalid Icmp type"


(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let rax_to_local (l:layout) (uid:uid) : ins list =
  let open Asm in
  [(Movq, [~%Rax; lookup l uid])]

(* XXX: puts the result into %rax. feasible for all cases ? *)
let compile_insn (ctxt:ctxt) ((uid:uid), (i:Ll.insn)) : X86.ins list =
  let { tdecls; layout; frame_size } = ctxt in
  begin match i with
    | Binop (bi, ty, op1, op2)
      -> if ty <> I64 then failwith "BOP only defined for I64 values" else
        compile_operand ctxt (Reg Rax) op1
        @ compile_operand ctxt (Reg Rcx) op2
        @ compile_bop ctxt bi
    | Alloca ty
      (* Alloca doesn't actually have to do anything *)
      -> [Movq, [lookup layout uid; Reg Rax]]
    | Load (ty, op)
      -> compile_operand ctxt (Reg Rax) op
        @ [Movq, [Ind2 Rax; Reg Rax]]
    | Store (ty, op1, op2)
      -> compile_operand ctxt (Reg Rax) op1
        @ compile_operand ctxt (Reg Rcx) op2
        @ [Movq, [Reg Rax; Ind2 Rcx]]
    | Icmp (cnd, ty, op1, op2)
      -> compile_operand ctxt (Reg Rax) op1
      @ compile_operand ctxt (Reg Rcx) op2
      @ compile_icmp ctxt cnd ty
    | Call (ty, op, argl)
      -> compile_call ctxt ty op argl
    | Bitcast (ty1, op, ty2)
      -> compile_operand ctxt (Reg Rax) op
    | Gep (ty, op, opl)
      -> compile_gep ctxt (ty, op) opl
  end
  @ rax_to_local layout uid


(* compiling terminators  --------------------------------------------------- *)

(* prefix the function name [fn] to a label to ensure that the X86 labels are
   globally unique . *)
let mk_lbl (fn:string) (l:string) = fn ^ "." ^ l

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional

   [fn] - the name of the function containing this terminator
*)
(* XXX: how do we return aggregate types? *)
let compile_terminator (fn:string) (ctxt:ctxt) (t:Ll.terminator) : ins list =
  let { tdecls; layout; frame_size } = ctxt in
  let open Asm in
  let stackframe_epilogue = [
    (Addq, [~$frame_size; ~%Rsp]);
    (Popq, [~%Rbp]);
    (Retq, [])
  ] in
  let jump_label lbl = [ Jmp, [~$$(mk_lbl fn lbl)] ] in
  let put_retval v = match v with
    | None -> []
    | Some op -> compile_operand ctxt ~%Rax op
  in match t with
  | Ret (tp, op_opt) -> (put_retval op_opt) @ stackframe_epilogue
	(* lookup block associated with l in CFG, set is as current executing block *)
	(* XXX: lookup in here impossible, insert Imm Lbls ? *)
  | Br l -> jump_label l
  (* if op = 1 then jump to l1 else jump to l2 *)
  | Cbr (op, l1, l2) -> match op with
    | Null -> jump_label l2
    | Const i -> if i = 1L then jump_label l1 else jump_label l2
    | Gid g -> [ Cmpq, [~$1; ~$$(Platform.mangle g)]
               ; J Neq, [~$$(mk_lbl fn l2)]
               ; Jmp, [~$$(mk_lbl fn l1)]
               ]
    | Id u -> [ Cmpq, [~$1; lookup layout u]
              ; J Neq, [~$$(mk_lbl fn l2)]
              ; Jmp, [~$$(mk_lbl fn l1)]
              ]

(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete.
   [fn] - the name of the function containing this block
   [ctxt] - the current context
   [blk]  - LLVM IR code for the block
*)
let compile_block (fn:string) (ctxt:ctxt) (blk:Ll.block) : ins list =
  (* TODO: This is just a temporary implementation, it doesn't actually work *)
  (* XXX: what is the uid for ? *)
  let { insns = insns; term = (uid, term)} = blk in
  (List.map (compile_insn ctxt) insns |> List.concat)
  @ compile_terminator fn ctxt term

let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)


(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : operand =
  match n with
    | 0 -> Reg Rdi
    | 1 -> Reg Rsi
    | 2 -> Reg Rdx
    | 3 -> Reg Rcx
    | 4 -> Reg R08
    | 5 -> Reg R09
    | i -> let displacement = ((i - 6) + 2) * 8 in Ind3(Lit (Int64.of_int displacement), Rbp)


(* We suggest that you create a helper function that computes the
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id
     is also stored as a stack slot.
   - see the discussion about locals

*)
let extract_uids ({ insns; term }:block) : uid list =
  List.split insns |> fst

let stack_layout (args : uid list) ((block, lbled_blocks):cfg) : layout =
  let all_blocks = List.split lbled_blocks |> snd |> List.cons block in
  let all_uids = List.map extract_uids all_blocks |> List.flatten |> (@) args in

  let calc_offset n = (-8) * (n + 1) |> Int64.of_int in
  let offset_to_operand o = Ind3(Lit o, Rbp) in
  let offsets = List.init (List.length all_uids) calc_offset in
  let operands = List.map offset_to_operand offsets in

  List.combine all_uids operands

let get_alloca_types (uid, insn) =
  match insn with
    | Alloca ty -> [(uid, ty)]
    | _ -> []

let rec scanl (f : 'b -> 'a -> 'b) (x : 'b) (xs : 'a list) : 'b list =
  match xs with
    | [] -> [x]
    | l::ls -> x :: (scanl f (f x l) ls)

let rec drop n l =
  match l with
  | x::xs when n > 0 -> drop (n-1) xs
  | _ -> l

let mem_layout (tdecls:(tid * ty) list) (layout:layout) ((block, lbled_blocks):cfg) : int * (uid * int) list =
  let locals_size = (List.length layout) * 8 in

  let all_blocks = List.split lbled_blocks |> snd |> List.cons block in
  let all_insns = List.map (fun { insns; term } -> insns) all_blocks |> List.flatten in
  let alloca_types = List.map get_alloca_types all_insns |> List.flatten in

  let uid_widths = List.map (fun (uid, ty) -> (uid, size_ty tdecls ty)) alloca_types in
  let (uids, widths) = List.split uid_widths in

  let mem_size = sum widths in
  (* If widths of the types are e.g.  8, 24, 32,  8
     then the offsets should be      -8,-32,-64,-72 *)
  let offsets = scanl (+) 0 widths |> drop 1 |> List.map ((+) locals_size) |> List.map Int.neg in

  (mem_size, List.combine uids offsets)


(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl (tdecls:(tid * ty) list) (name:string) ({ f_ty; f_param; f_cfg }:fdecl) : prog =
  let layout = stack_layout f_param f_cfg in
  let (mem_size, mem_layout) = mem_layout tdecls layout f_cfg in

  (* Calculate stack frame size *)
  let frame_size = (List.length layout) * 8 + mem_size in
  let ctxt = { tdecls; layout; frame_size } in

  (* Move function parameters into corresponding stack slots *)
  let params = List.combine (List.init (List.length f_param) Fun.id) f_param in
  let f (n, uid) = interm_mov (arg_loc n) (lookup layout uid) in
  let param_moves = List.map f params |> List.concat in

  (* Make pointers point to correct memory locations *)
  let set_ptr_of_uid (uid, offset) = [ Leaq, [Ind3 (Lit (Int64.of_int offset), Rbp); Reg R10]
                                     ; Movq, [Reg R10; lookup layout uid]] in
  let set_mem_ptrs = List.map set_ptr_of_uid mem_layout |> List.flatten in

  (* Construct function prelude *)
  let function_prelude : ins list =
    let open Asm in
    [ Pushq, [~%Rbp]
    ; Movq, [~%Rsp; ~%Rbp]
    ; Subq, [~$frame_size; ~%Rsp]
    ] @ param_moves @ set_mem_ptrs
  in

  (* Compile first block and append to function prelude *)
  let (fblock, lbled_blocks) = f_cfg in
  let fblock_prog = compile_block name ctxt fblock in
  let progtext = function_prelude @ fblock_prog in
  let elem = {
    lbl = name;
    global = true;
    asm = Text progtext;
  } in

  (* Compile labeled blocks *)
  let g (lbl, block) = compile_lbl_block name lbl ctxt block in
  let compiled_blocks = List.map g lbled_blocks in

  elem :: compiled_blocks



(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (t1,g,t2) -> compile_ginit g

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
