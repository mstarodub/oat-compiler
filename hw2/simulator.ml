(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8                 (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool (* overflow *)
             ; mutable fs : bool (* sign *)
             ; mutable fz : bool (* zero *)
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun x ->
  let e_eq = fz in
  let e_lt = fs <> fo in
  begin match x with
    | Eq -> e_eq
    | Neq -> not e_eq
    | Lt -> e_lt
    | Le -> e_lt || e_eq
    | Gt -> not (e_lt || e_eq)
    | Ge -> not e_lt
  end

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  let open Int64 in
  match sub addr mem_bot with
    | i when (compare i 0L) <= -1 -> None
    | i when i >= (sub mem_top mem_bot) -> None
    | i -> Some (to_int i)

(* useful for instrucions which do not update the flags *)
let old_flags (m:mach) : flags =
  m.flags

(* sets the machine flags to new ones *)
let update_flags (m:mach) (f:flags) : unit =
  m.flags.fo <- f.fo;
  m.flags.fs <- f.fs;
  m.flags.fz <- f.fz

(* interp_cnd wrapper for convenience *)
let interp_cnd_m (m:mach) : cnd -> bool =
  interp_cnd (old_flags m)

(* returns the int64 value a register is holding *)
let reg_read (m:mach) (r:reg) : int64 =
  m.regs.(rind r)

(* updates the int64 value a register is holding *)
let reg_write (m:mach) (r:reg) (v:int64) : unit =
  m.regs.(rind r) <- v

(* returns the ins_size symbolic bytes within memory at the given address *)
(* may raise X86lite_segfault *)
let mem_read (m:mach) (addr:quad) : sbyte list =
  match map_addr addr with
  | None -> raise X86lite_segfault
  | Some ind -> Array.to_list @@ Array.sub m.mem ind ins_size

(* updates the ins_size symbolic bytes within memory at the given address *)
(* may raise X86lite_segfault *)
let mem_write (m:mach) (addr:quad) (bs:sbyte list) : unit =
  match map_addr addr with
  | None -> raise X86lite_segfault
  | Some ind
    -> let arr = Array.of_list bs in
      Array.blit arr 0 m.mem ind ins_size

(* returns the ins_size sbytes within memory at the given register treated as ptr *)
let mem_read_atreg (m:mach) (r:reg) : sbyte list =
  mem_read m @@ reg_read m r

(* updates the ins_size sbytes within memory at the given register treated as ptr *)
let mem_write_atreg (m:mach) (r:reg) (bs:sbyte list) : unit =
  mem_write m (reg_read m r) bs

(* returns the ins_size sbytes within memory at the given register plus displacement treated as ptr *)
let mem_read_atreg_disp (m:mach) (r:reg) (d:quad) : sbyte list =
  mem_read m @@ Int64.add d (reg_read m r)

(* updates the ins_size sbytes within memory at the given register plus displacement treated as ptr *)
let mem_write_atreg_disp (m:mach) (r:reg) (d:quad) (bs:sbyte list) : unit =
  mem_write m (Int64.add d (reg_read m r)) bs

let decode_imm (i:imm) : sbyte list =
  match i with
  | Lit q -> sbytes_of_int64 q
  | Lbl l -> sbytes_of_string l

let decode_imm_asquad (i:imm) : int64 =
  int64_of_sbytes @@ decode_imm i

let decode_src (m:mach) (o:operand) : sbyte list =
  match o with
  | Imm im -> decode_imm im
  | Reg r -> sbytes_of_int64 @@ reg_read m r
  | Ind1 im -> mem_read m (decode_imm_asquad im)
  | Ind2 r -> mem_read_atreg m r
  | Ind3 (im, r) -> mem_read_atreg_disp m r (decode_imm_asquad im)

type retval
  = Memory of quad * sbyte list
  | Register of reg * int64
  | Neither

let decode_dest (m:mach) (o:operand) : retval =
  let dummy_r = 0L and dummy_m = sbytes_of_int64 0L in
  match o with
  | Imm im -> Neither
  | Reg r -> Register (r, dummy_r)
  | Ind1 im -> Memory (decode_imm_asquad im, dummy_m)
  | Ind2 r -> Memory (reg_read m r, dummy_m)
  | Ind3 (im, r) -> Memory (Int64.add (decode_imm_asquad im) (reg_read m r), dummy_m)

let multi_dec_srcsrcdest (m:mach) (o:operand) : sbyte list * int64 * retval =
  let sbs : sbyte list = decode_src m o in
  (sbs, int64_of_sbytes sbs, decode_dest m o)

(* packs an int64 into one that will be used for updating depening on dest *)
let pack_ret (re:retval) (v:int64) : retval =
  match re with
  | Neither -> Neither
  | Memory (addr, _) -> Memory (addr, sbytes_of_int64 v)
  | Register (r, _) -> Register (r, v)

let arity_check (i:int) (os:operand list) : operand list =
  let failstr = "wrong number of ops for " ^ (string_of_int i) ^ "ary inst" in
  match os with
  | _::_ when i = 0 -> failwith failstr
  | [] | _::_::_ when i = 1 -> failwith failstr
  | [] | _::[] | _::_::_::_ when i = 2 -> failwith failstr
  | _ -> os

(* usual condition for setting the sign flag *)
let default_sign (i:int64) : bool =
  (compare i 0L) < 0

(* usual condition for setting the zero flag *)
let default_zero (i:int64) : bool =
  i = 0L

(* helper function for i_bitwise *)
let dispatch_bitwise_of_flag (o:opcode) (old_dest:int64) (new_dest:int64) : bool =
  let nthbit x n = Int64.logand x (Int64.shift_left 1L n) <> 0L in
  match o with
  | Sarq -> false
  | Shlq -> nthbit new_dest 63 <> nthbit new_dest 62
  | Shrq -> nthbit old_dest 63

(* TODO: check OF correctness *)
let i_arithm_unary (m:mach) f (os:operand list) : flags * retval list =
  let open Int64_overflow in
  let [o] = arity_check 1 os in
  let (_, iv, ds) = multi_dec_srcsrcdest m o in
  let {value = v; overflow = f_o} = f iv
  in
    ({ fo = f_o
    ; fs = default_sign v
    ; fz = default_zero v
    }, [pack_ret ds v])

(* TODO: check OF correctness *)
let i_arithm (m:mach) f (os:operand list) : flags * retval list =
  let open Int64_overflow in
  let [o1; o2] = arity_check 2 os in
  let (_, iv1, _) = multi_dec_srcsrcdest m o1 in
  let (_, iv2, ds) = multi_dec_srcsrcdest m o2 in
  let {value = v; overflow = f_o} = f iv2 iv1
  in
    ({ fo = f_o
    ; fs = default_sign v
    ; fz = default_zero v
    }, [pack_ret ds v])

let i_logic_unary (m:mach) f (os:operand list) : flags * retval list =
  let [o] = arity_check 1 os in
  let (_, iv, ds) = multi_dec_srcsrcdest m o in
  let v = f iv
  in
    (old_flags m, [pack_ret ds v])

let i_logic (m:mach) f (os:operand list) : flags * retval list =
  let [o1; o2] = arity_check 2 os in
  let (_, iv1, _) = multi_dec_srcsrcdest m o1 in
  let (_, iv2, ds) = multi_dec_srcsrcdest m o2 in
  let v = f iv2 iv1
  in
    ({ fo = false
    ; fs = default_sign v
    ; fz = default_zero v
    }, [pack_ret ds v])

let i_bitwise (m:mach) f g (os:operand list) : flags * retval list =
  let [o1; o2] = arity_check 2 os in
  let (_, iv1, _) = multi_dec_srcsrcdest m o1 in
  let (_, iv2, ds) = multi_dec_srcsrcdest m o2 in
  let amt = Int64.to_int iv1 in
  let v = f iv2 amt in
  (* apply partialy applied dispatcher to old dest and new dest *)
  let f_o = g iv2 v
  in
    if amt = 0 then
      (old_flags m, [pack_ret ds v])
    else
      ({ fo = if amt = 1 then f_o else (old_flags m).fo
      ; fs = default_sign v
      ; fz = default_zero v
      }, [pack_ret ds v])

let i_set (m:mach) (x:bool) (os:operand list) : flags * retval list =
  (* clear the lower bit with andmask 11111110, set it with or fillb *)
  let set_lbyte (a:int64) (b:int64) : int64 =
    Int64.logor (Int64.logand a (Int64.shift_left (-1L) 8)) b
  in
  let [o] = arity_check 1 os in
  let (_, iv, ds) = multi_dec_srcsrcdest m o in
  let fillb = if x then 1L else 0L in
  let v = set_lbyte iv fillb
  in
    (old_flags m, [pack_ret ds v])

let i_jmp (m:mach) (x:bool) (os:operand list) : flags * retval list =
  let [o] = arity_check 1 os in
  let (_, iv, _) = multi_dec_srcsrcdest m o
  in
    if x then
      (old_flags m, [Register (Rip, iv)])
    else
      (old_flags m, [])

let i_lea (m:mach) (os:operand list) : flags * retval list =
  let [o1; o2] = arity_check 2 os in
  let (_, _, ds) = multi_dec_srcsrcdest m o2 in
  let iv = match o1 with
    | Ind1 im -> decode_imm_asquad im
    | Ind2 r -> reg_read m r
    | Ind3 (im, r) -> Int64.add (reg_read m r) (decode_imm_asquad im)
    | _ -> failwith "lea: recieved a non-Ind src"
  in
    (old_flags m, [pack_ret ds iv])

let i_mov (m:mach) (os:operand list) : flags * retval list =
  let [o1; o2] = arity_check 2 os in
  let (_, iv, _) = multi_dec_srcsrcdest m o1 in
  let (_, _, ds) = multi_dec_srcsrcdest m o2
  in
    (old_flags m, [pack_ret ds iv])

(* [InsB0 (Pushq, [~$42]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag] *)
(* "rsp=4 *65520=2A" *)
(* (fun m -> m.regs.(rind Rsp) = 0x0040FFF0L *)
           (* && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 0x2AL *)
    (* ) *)
let i_push (m:mach) (os:operand list) : flags * retval list =
  let [o] = arity_check 1 os in
  let (sbs, _, _) = multi_dec_srcsrcdest m o
  in
    (old_flags m,
    [Register (Rsp, Int64.sub (reg_read m Rsp) 8L);
    (* doesnt do shit -> pushq failing (mem_size-8 passing)*)
      Memory (reg_read m Rsp, sbs)])

let i_pop (m:mach) (os:operand list) : flags * retval list =
  let [o] = arity_check 1 os in
  let (_, _, ds) = multi_dec_srcsrcdest m o
  in
    (old_flags m,
    [pack_ret ds (int64_of_sbytes @@ mem_read_atreg m Rsp);
      Register (Rsp, Int64.add (reg_read m Rsp) 8L)])

let i_cmp (m:mach) (os:operand list) : flags * retval list =
  let open Int64_overflow in
  let [o1; o2] = arity_check 2 os in
  let (_, iv1, _) = multi_dec_srcsrcdest m o1 in
  let (_, iv2, _) = multi_dec_srcsrcdest m o2 in
  let {value = v; overflow = f_o} = sub iv2 iv1
  in
    ({ fo = f_o
    ; fs = default_sign v
    ; fz = default_zero v
    }, [])

(* group by arity, compute source/dest inside the simulation *)
(* each simulation group returns flag triple and a list of dest vals needing update *)
let rec simulate_inst (m:mach) (o:opcode) (os:operand list) : flags * retval list =
  match o with
  (* 1ary artihm 64 *)
  | Negq -> i_arithm_unary m Int64_overflow.neg os
  | Incq -> i_arithm_unary m Int64_overflow.succ os
  | Decq -> i_arithm_unary m Int64_overflow.pred os
  (* 2ary artihm 64 *)
  | Addq -> i_arithm m Int64_overflow.add os
  | Subq -> i_arithm m Int64_overflow.sub os
  | Imulq -> i_arithm m Int64_overflow.mul os
  (* 1ary logic *)
  | Notq -> i_logic_unary m Int64.lognot os
  (* 2ary logic *)
  | Andq -> i_logic m Int64.logand os
  | Orq -> i_logic m Int64.logor os
  | Xorq -> i_logic m Int64.logxor os
  (* 2ary bitwise *)
  | Sarq -> i_bitwise m Int64.shift_right (dispatch_bitwise_of_flag o) os
  | Shlq -> i_bitwise m Int64.shift_left (dispatch_bitwise_of_flag o) os
  | Shrq -> i_bitwise m Int64.shift_right_logical (dispatch_bitwise_of_flag o) os
  (* dealing with condition codes *)
  | Set cn -> i_set m (interp_cnd_m m cn) os
  | J cn -> i_jmp m (interp_cnd_m m cn) os
  | Leaq -> i_lea m os
  | Movq -> i_mov m os
  (* | Pushq -> i_push m os *)
  (* retarded version because other one is buggy *)
  | Pushq -> i_push_retard m os
  | Popq -> i_pop m os
  | Cmpq -> i_cmp m os
  | Jmp -> simulate_inst m Movq (os @ [Reg Rip])
  (* concat the retval lists *)
  | Callq ->
    let f = old_flags m in
    let (_, l1) = simulate_inst m Pushq [Reg Rip] in
    let (_, l2) = simulate_inst m Jmp os in
    (f, l1 @ l2)
  | Retq -> simulate_inst m Popq [Reg Rip]
and i_push_retard (m:mach) (os:operand list) : flags * retval list =
  let f = old_flags m in
  let (_, l1) = simulate_inst m Decq [Reg Rsp] in
  let (_, l2) = simulate_inst m Decq [Reg Rsp] in
  let (_, l3) = simulate_inst m Decq [Reg Rsp] in
  let (_, l4) = simulate_inst m Decq [Reg Rsp] in
  let (_, l5) = simulate_inst m Decq [Reg Rsp] in
  let (_, l6) = simulate_inst m Decq [Reg Rsp] in
  let (_, l7) = simulate_inst m Decq [Reg Rsp] in
  let (_, l8) = simulate_inst m Decq [Reg Rsp] in
  let (_, l9) = simulate_inst m Movq (os @ [Ind2 Rsp]) in
  (f, l1 @ l2 @ l3 @ l4 @ l5 @ l6 @ l7 @ l8 @ l9)

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
let step (m:mach) : unit =
  let mem_conts : sbyte list = mem_read_atreg m Rip in
  let (newflags, newvals) : flags * retval list =
    match List.hd mem_conts with
    | InsFrag -> failwith "not an instruction byte: got InsFrag"
    | Byte _ -> failwith "not an instruction byte: got raw Byte"
    | InsB0 (oc, ops) -> simulate_inst m oc ops
  in
    (* according to type, update appropriate dest memory or register *)
    List.iter (begin
      fun n -> match n with
      | Neither -> ()
      | Register (r, v) -> reg_write m r v
      | Memory (addr, bs) -> mem_write m addr bs
    end) newvals;
    (* set flags *)
    update_flags m newflags;
    (* advance Rip *)
    reg_write m Rip @@ Int64.add (reg_read m Rip) (Int64.of_int ins_size)

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 =
  while reg_read m Rip <> exit_addr do step m done;
  reg_read m Rax

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
