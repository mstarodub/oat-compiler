open Assert
open Hellocaml
open Randomast

(* These tests are provided by you -- they will NOT be graded *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let e1 : exp = Mult(Const 2L, Const 3L)   (* "2 * 3" *)
let e2 : exp = Add(Var "x", Const 1L)    (* "x + 1" *)
let e3 : exp = Mult(Var "y", Mult(e2, Neg e2))     (* "y * ((x+1) * -(x+1))" *)
let ctxt1 : ctxt = [("x", 3L)]             (* maps "x" to 3L *)
let ctxt2 : ctxt = [("x", 2L); ("y", 7L)]  (* maps "x" to 2L, "y" to 7L *)


let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 25) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) 64);
  ]);
  Test ("student tests for 2-1", [
    ("case1", assert_eq 3 @@ third_of_three (1, 2, 3));
  ]);
  Test ("student tests for problem 4-5 (optimiser)", [
    let (c, e) = rnd_ctx_exp_pair_p 11 in
      ("opt_case_random_1", assert_eqf (fun () -> (interpret c (optimize e))) (interpret c e));
    let (c, e) = rnd_ctx_exp_pair_p 22 in
      ("opt_case_random_2", assert_eqf (fun () -> (interpret c (optimize e))) (interpret c e));
    let (c, e) = rnd_ctx_exp_pair_p 33 in
      ("opt_case_random_3", assert_eqf (fun () -> (interpret c (optimize e))) (interpret c e));
    let (c, e) = rnd_ctx_exp_pair_p 44 in
      let optimized = optimize e in begin
      print_string ("OPTIMISED AST: " ^ (string_of optimized) ^ "\n");
      ("opt_case_random_4", assert_eqf (fun () -> (interpret c optimized)) (interpret c e));
      end
  ]);
  (let corr_tf ((c:ctxt), (e:exp)) : bool = let compiled = compile e in begin
    (* dump_stack compiled; *)
    (interpret c e) = (run c compiled)
    end
  in Test ("student tests for problem 5 (compiler)", [
    ("compile_case1", assert_eq true @@ corr_tf ([], e1));
    ("compile_case2", assert_eq true @@ corr_tf (ctxt1, e2));
    ("compile_case3", assert_eq true @@ corr_tf (ctxt2, e3));
    ("compile_case_random_1", assert_eq true @@ corr_tf (rnd_ctx_exp_pair_p 1));
    ("compile_case_random_2", assert_eq true @@ corr_tf (rnd_ctx_exp_pair_p 2));
    ("compile_case_random_3", assert_eq true @@ corr_tf (rnd_ctx_exp_pair_p 3));
    ("compile_case_random_4", assert_eq true @@ corr_tf (rnd_ctx_exp_pair_p 4));
    ("compile_case_random_5", assert_eq true @@ corr_tf (rnd_ctx_exp_pair_p 5));
  ]));
]
