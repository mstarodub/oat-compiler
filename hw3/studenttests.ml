open Assert

(* These tests are provided by you -- they will not be graded  *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let calling = [
  (* https://godbolt.org/z/9nGb1n *)
  "tests/lol.ll", 10L
]

let llprogs =
  [ "llprograms/add_twice.ll", 29L
  ; "llprograms/analysis10_cf_opt.ll", 60L
  ; "llprograms/analysis10_dce_opt.ll", 60L
  ; "llprograms/analysis10.ll", 60L
  ; "llprograms/analysis11_cf_opt.ll", 3L
  ; "llprograms/analysis11_dce_opt.ll", 3L
  ; "llprograms/analysis11.ll", 3L
  ; "llprograms/analysis12_cf_opt.ll", 14L
  ; "llprograms/analysis12_dce_opt.ll", 14L
  ; "llprograms/analysis12.ll", 14L
  ; "llprograms/analysis13_cf_opt.ll", 7L
  ; "llprograms/analysis13_dce_opt.ll", 7L
  ; "llprograms/analysis13.ll", 7L
  ; "llprograms/analysis14_cf_opt.ll", 42L
  ; "llprograms/analysis14_dce_opt.ll", 42L
  ; "llprograms/analysis14.ll", 42L
  ; "llprograms/analysis15_cf_opt.ll", 2L
  ; "llprograms/analysis15_dce_opt.ll", 2L
  ; "llprograms/analysis15.ll", 2L
  ; "llprograms/analysis16_cf_opt.ll", 1L
  ; "llprograms/analysis16_dce_opt.ll", 1L
  ; "llprograms/analysis16.ll", 1L
  ; "llprograms/analysis18_cf_opt.ll", 42L
  ; "llprograms/analysis18_dce_opt.ll", 42L
  ; "llprograms/analysis18.ll", 42L
  ; "llprograms/analysis19_cf_opt.ll", 5L
  ; "llprograms/analysis19_dce_opt.ll", 5L
  ; "llprograms/analysis19.ll", 5L
  ; "llprograms/analysis1_cf_opt.ll", 49L
  ; "llprograms/analysis1_dce_opt.ll", 49L
  ; "llprograms/analysis1.ll", 49L
  ; "llprograms/analysis2_cf_opt.ll", 8L
  ; "llprograms/analysis2_dce_opt.ll", 8L
  ; "llprograms/analysis2.ll", 8L
  ; "llprograms/analysis3_cf_opt.ll", 188L
  ; "llprograms/analysis3_dce_opt.ll", 188L
  ; "llprograms/analysis3.ll", 188L
  ; "llprograms/analysis4_cf_opt.ll", 254L
  ; "llprograms/analysis4_dce_opt.ll", 254L
  ; "llprograms/analysis4.ll", 254L
  ; "llprograms/analysis5_cf_opt.ll", 14L
  ; "llprograms/analysis5_dce_opt.ll", 14L
  ; "llprograms/analysis5.ll", 14L
  ; "llprograms/analysis7_cf_opt.ll", 10L
  ; "llprograms/analysis7_dce_opt.ll", 10L
  ; "llprograms/analysis7.ll", 10L
  ; "llprograms/analysis8_cf_opt.ll", 95L
  ; "llprograms/analysis8_dce_opt.ll", 95L
  ; "llprograms/analysis8.ll", 95L
  ; "llprograms/analysis9_cf_opt.ll", 0L
  ; "llprograms/analysis9_dce_opt.ll", 0L
  ; "llprograms/analysis9.ll", 0L
  ; "llprograms/arith_combo_dce.ll", 4L
  ; "llprograms/arith_combo_fold.ll", 4L
  ; "llprograms/arith_combo.ll", 4L
  ; "llprograms/ashu_ian_opt.ll", 5L
  ; "llprograms/binary_gcd.ll", 3L
  ; "llprograms/binarysearch.ll", 8L
  ; "llprograms/call7.ll", 7L
  ; "llprograms/call8.ll", 21L
  ; "llprograms/cbr3.ll", 9L
  ; "llprograms/certified_random_number_generator.ll", 4L
  ; "llprograms/certified_random_number_generator_soln.ll", 4L
  ; "llprograms/euclid.ll", 2L
  ; "llprograms/gcd_euclidian.ll", 2L
  (* ; "llprograms/gep10.ll", 3L *)
  (* ; "llprograms/gep9.ll", 5L *)
  ; "llprograms/kaiterry_pi.ll", 0L
  ; "llprograms/kaiterry_pi_opt.ll", 0L
  (* ; "llprograms/kaiterry_units.ll", 1L *)
  ; "llprograms/kaiterry_units_opt.ll", 1L
  ; "llprograms/kierajmumick.ll", 164L
  ; "llprograms/kierajmumickopt.ll", 164L
  ; "llprograms/lfsr.ll", 108L
  ; "llprograms/linear_search.ll", 1L
  ; "llprograms/matmul.ll", 0L
  ; "llprograms/max_thomas_test.ll", 120L
  ; "llprograms/max_thomas_test_opt.ll", 120L
  ; "llprograms/naive_factor_nonprime.ll", 0L
  ; "llprograms/naive_factor_prime.ll", 1L
  ; "llprograms/opt_cbr_test1.ll", 0L
  ; "llprograms/opt_globals_test1.ll", 1L
  ; "llprograms/opt_globals_test1_soln.ll", 1L
  ; "llprograms/qtree.ll", 3L
  ; "llprograms/reed_nate_opt.ll", 196L
  ; "llprograms/regtest1.ll", 254L
  ; "llprograms/return_intermediate_dce.ll", 18L
  ; "llprograms/return_intermediate_fold.ll", 18L
  ; "llprograms/return_intermediate.ll", 18L
  ; "llprograms/returnvoid.ll", 16L
  ; "llprograms/sub_neg_dce.ll", 255L
  ; "llprograms/sub_neg_fold.ll", 255L
  ; "llprograms/sub_neg.ll", 255L
  ; "llprograms/sum_tree.ll", 116L
  ; "llprograms/vivekraj_jjlee.ll", 1L
  ; "llprograms/vivekraj_jjlee_opt.ll", 1L
  ; "llprograms/sp20_hw3/slowsort.ll", 155L
  ; "llprograms/sp20_hw3/sum_half_list.ll", 15L
  (* ; "llprograms/sp20_hw3/tarjans_toposort.ll", 1L *)
  ; "llprograms/sp20_hw3/tree_search.ll", 1L
  ; "llprograms/sp20_hw3/countsort.ll", 6L
  ; "llprograms/sp20_hw3/det3x3.ll", 11L
  ; "llprograms/sp20_hw3/editdistance.ll", 7L
  ; "llprograms/sp20_hw3/fibonacci.ll", 8L
  ; "llprograms/sp20_hw3/find_max.ll", 80L
  ; "llprograms/sp20_hw3/fp32lite.ll", 254L
  (* ; "llprograms/sp20_hw3/insertion.ll", 11L *)
  ; "llprograms/sp20_hw3/log.ll", 4L
  ; "llprograms/sp20_hw3/many_calls.ll", 0L
  ; "llprograms/sp20_hw3/ncr.ll", 84L
  ; "llprograms/sp20_hw3/our_test.ll", 255L
  ]

let llio =
  [ "llprograms/sp20_hw3/another_io_test.ll", [], "Hello, world!(null) 2020 3 4 5 6 7 8"
  ; "llprograms/sp20_hw3/heapsort.ll", [], "Sum of first element of each list before sorting: 15; sum after: -6; elements of arr2 after sort: -5 1 5 5 6 7 10 49 84 741 4512 4851"
  ; "llprograms/sp20_hw3/quicksort.ll", [], "Initial array: 4 1 50 50 7 0 5 10 9 11 0 100 3 8; Sorted array: 0 0 1 3 4 5 7 8 9 10 11 50 50 100"
  ]

let provided_tests : suite = [
  Test ("student test - calling stuff", Gradedtests.executed calling);
  Test ("student test - regular llprogs", Gradedtests.executed llprogs);
  Test ("student test - llprogs io", Gradedtests.executed_io llio);
]
