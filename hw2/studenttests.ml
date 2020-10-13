open Assert
open X86
open Simulator
open Asm

(* You can use this file for additional test cases to help your *)
(* implementation.                                              *)


(* let provided_tests : suite = [ *)
  (* Test ("Debug", [ *)
  (* ]); *)
(*  *)
(* ] *)

let shr =
Gradedtests.test_machine
[
InsB0 (Movq, [ ~$16; ~%Rax ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$16; Gradedtests.stack_offset 0L ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$(-16); Gradedtests.stack_offset (-8L) ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$3; ~%Rcx ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Shrq, [ ~$1; ~%Rax ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Shrq, [ ~%Rcx; Gradedtests.stack_offset 0L ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Shrq, [ ~%Rcx; Gradedtests.stack_offset (-8L) ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
]

let sar =
Gradedtests.test_machine
[
InsB0 (Movq, [ ~$16; ~%Rax ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$16; Gradedtests.stack_offset 0L ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$(-16); Gradedtests.stack_offset (-8L) ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Movq, [ ~$3; ~%Rcx ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Sarq, [ ~$1; ~%Rax ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Sarq, [ ~%Rcx; Gradedtests.stack_offset 0L ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsB0 (Sarq, [ ~%Rcx; Gradedtests.stack_offset (-8L) ]);
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
InsFrag;
]



let bit_manipulation = [
  ( "shr",
    Gradedtests.machine_test "rax=8 *65528=2 *65520=0x2000000000000000" 7 shr
    (fun m -> m.regs.(rind Rax) = 8L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size - 8)) = 2L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size - 16)) = 2305843009213693950L)
  );
  ( "sar",
    Gradedtests.machine_test "rax=8 *65528=2 *65520=-2" 7 sar
    (fun m -> m.regs.(rind Rax) = 8L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size - 8)) = 2L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size - 16)) = -2L)
  );
]

let iter_fac = [
  ( "iterfac",
    Gradedtests.program_test (Gradedtests.factorial_iter 6)
    720L
  );
]

let provided_tests : suite = [ Test ("student tests - Bit manipulation", bit_manipulation);
Test ("student tests - factorial iterative", iter_fac)
 ]


