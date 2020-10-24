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
  (Movq, [o; Reg R09]);
  (Movq, [Reg R09; dest])
]

let compile_operand (ctxt:ctxt) (dest:X86.operand) : Ll.operand -> ins list =
  let { tdecls; layout} = ctxt in
  let open Asm in
  function ll_op -> match ll_op with
    | Null -> [(Movq, [~$0; dest])]
    | Const i -> [(Movq, [Imm (Lit i); dest])]
    | Gid g -> failwith "globals unimplemented"
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
let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
failwith "compile_gep not implemented"

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

let compile_icmp ({ tdecls; layout }:ctxt) (cnd:Ll.cnd) (ty:Ll.ty) =
  let open Asm in
  if ty = I64 then
    let x86_cnd = compile_cnd cnd in
    [ Cmpq, [~%Rcx; ~%Rax]
    (* Clear rax value (setb only sets lower byte) *)
    ; Movq, [~$0; ~%Rax]
    ; Set x86_cnd, [~%Rax]
    ]
  else
    (* XXX: Does icmp compare aggregate values or only the pointers? *)
    failwith "compile_icmp unimplemented for non-I64 values"


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
  let { tdecls; layout} = ctxt in
  begin match i with
    | Binop (bi, ty, op1, op2)
      -> if ty <> I64 then failwith "BOP only defined for I64 values" else
        compile_operand ctxt (Reg Rax) op1
       @ compile_operand ctxt (Reg Rcx) op2
       @ compile_bop ctxt bi
    | Alloca ty
      -> failwith "unimplemented Alloca instruction"
    | Load (ty, op)
      -> failwith "unimplemented Load instruction"
    | Store (ty, op1, op2)
      -> failwith "unimplemented Store instruction"
    | Icmp (cnd, ty, op1, op2)
      -> compile_operand ctxt (Reg Rax) op1
      @ compile_operand ctxt (Reg Rcx) op2
      @ compile_icmp ctxt cnd ty
    | Call (ty, op, argl)
      -> failwith "unimplemented Call instruction"
    | Bitcast (ty1, op, ty2)
      -> []
    | Gep (ty, op, opl)
      -> failwith "unimplemented Gep instruction"
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
  let { tdecls; layout} = ctxt in
  let locals_size = (List.length layout) * 8 in
  let open Asm in
  let stackframe_epilogue = [
    (Addq, [~$locals_size; ~%Rsp]);
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
  (* TODO: Maybe replace this with compile_operand later? *)
  | Cbr (op, l1, l2) -> match op with
    | Null -> jump_label l2
    | Const i -> if i = 1L then jump_label l1 else jump_label l2
    | Gid g -> failwith "globals unimplemented"
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
  let { tdecls; layout} = ctxt in
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
  let ctxt = { tdecls; layout } in

  (* Calculate stack frame size *)
  let locals_size = (List.length layout) * 8 in

  (* Move function parameters into corresponding stack slots *)
  let params = List.combine (List.init (List.length f_param) Fun.id) f_param in
  let f (n, uid) = interm_mov (arg_loc n) (lookup layout uid) in
  let param_moves = List.map f params |> List.concat in

  (* Construct function prelude *)
  let function_prelude : ins list =
    let open Asm in
    [ Pushq, [~%Rbp]
    ; Movq, [~%Rsp; ~%Rbp]
    ; Subq, [~$locals_size; ~%Rsp]
    ] @ param_moves
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
