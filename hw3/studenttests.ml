open Assert

(* These tests are provided by you -- they will not be graded  *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let calling = [
  (* https://godbolt.org/z/9nGb1n *)
  "tests/lol.ll", 10L
]

let provided_tests : suite = [
  Test ("student test - calling stuff", Gradedtests.executed calling)
]
