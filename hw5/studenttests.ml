open Assert

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let tc_ok_tests = [
  "test/ok1.oat"
  ; "test/ok2.oat"
  ; "test/zeroarg.oat"
]

let tc_err_tests = [
  "test/fail1.oat"
  ; "test/fail2.oat"
  ; "test/redeclarelocal.oat"
  ; "test/minilocalfail1.oat"
  ; "test/minilocalfail2.oat"
  ; "test/minilocalfail3.oat"
]

let run_tests = [
  (* ("test/ifq.oat", "", "Hello") *)
]

let provided_tests : suite = [
  Test("student test - tc ok tests", Gradedtests.typecheck_file_error tc_err_tests);
  Test("student test - tc err tests", Gradedtests.typecheck_file_correct tc_ok_tests);
  Test("student test - executed", Gradedtests.executed_oat_file run_tests);
]
