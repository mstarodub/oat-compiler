open Assert

(* These tests are provided by you *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let scmp s : string list =
  List.filter ((<>) "") @@
  List.concat @@
  List.map (String.split_on_char '\n') @@
  String.split_on_char ' '
  s

let rec print_list s =
  match s with
  | [] -> ()
  | hd::tl -> print_string hd; print_string "\" \""; print_list tl

let roundtrip path : bool =
  let oat_ast = Driver.parse_oat_file path in
  let orig_oat = String.trim @@ Driver.read_file path in
  let parsed_oat = String.trim @@ Astlib.string_of_prog oat_ast in
  let res = (scmp parsed_oat) = (scmp orig_oat) in
  if not res then begin
    Driver.print_banner ("file: " ^ path);
    print_endline orig_oat;
    Driver.print_banner "parsed:";
    print_endline parsed_oat
  end;
  res

let do_roundtr path =
  let path' = "oatprograms/" ^ path in
  (path', assert_eqf (fun () -> roundtrip path') true)

let parsetests = [
  do_roundtr "fact.oat";
  (* do_roundtr "calculator.oat"; *)
  (* do_roundtr "float_multiply.oat"; *)
  (* do_roundtr "kmp.oat"; *)
  (* do_roundtr "leastsquare.oat"; *)
  (* do_roundtr "lfsr2.oat"; *)
  (* do_roundtr "lib10.oat"; *)
  (* do_roundtr "lib12.oat"; *)
  (* do_roundtr "lib13.oat"; *)
  (* do_roundtr "lib2.oat"; *)
  (* do_roundtr "lib3.oat"; *)
  (* do_roundtr "no_of_fac.oat"; *)
  (* do_roundtr "qs_bs.oat"; *)
  (* do_roundtr "regalloctest2.oat"; *)
  (* do_roundtr "regalloctest.oat"; *)
  (* do_roundtr "regex.oat"; *)
  (* do_roundtr "reverse.oat"; *)
  (* do_roundtr "rod_cutting.oat"; *)
  (* do_roundtr "run51.oat"; *)
  (* do_roundtr "run52.oat"; *)
  (* do_roundtr "run54.oat"; *)
  (* do_roundtr "run55.oat"; *)
  (* do_roundtr "run56.oat"; *)
  (* do_roundtr "runtime-fail1.oat"; *)
  (* do_roundtr "runtime-fail2.oat"; *)
  (* do_roundtr "runtime-fail3.oat"; *)
  (* do_roundtr "runtime-fail4.oat"; *)
  (* do_roundtr "setg.oat"; *)
  (* do_roundtr "shortest_path_matrix.oat"; *)
  (* do_roundtr "tc10.oat"; *)
  (* do_roundtr "tc11.oat"; *)
  (* do_roundtr "tc13.oat"; *)
  (* do_roundtr "tc14.oat"; *)
  (* do_roundtr "tc15.oat"; *)
  (* do_roundtr "tc16.oat"; *)
  (* do_roundtr "tc17.oat"; *)
  (* do_roundtr "tc18.oat"; *)
  (* do_roundtr "tc19.oat"; *)
  (* do_roundtr "tc1.oat"; *)
  (* do_roundtr "tc20.oat"; *)
  (* do_roundtr "tc2.oat"; *)
  (* do_roundtr "tc3.oat"; *)
  (* do_roundtr "tc4.oat"; *)
  (* do_roundtr "tc5.oat"; *)
  (* do_roundtr "tc6.oat"; *)
  (* do_roundtr "tc7.oat"; *)
  (* do_roundtr "tc9.oat"; *)
  (* do_roundtr "tc_ok1.oat"; *)
  (* do_roundtr "tc_ok2.oat"; *)
  (* do_roundtr "tc_ok5.oat"; *)
  (* do_roundtr "tc_ok6.oat"; *)
  (* do_roundtr "tc_ok7.oat"; *)
  (* do_roundtr "tc_ok8.oat"; *)
  (* do_roundtr "toascii.oat"; *)
  (* do_roundtr "union_find.oat"; *)
  (* do_roundtr "xor_bool.oat"; *)
  (* OK *) (* do_roundtr "abs.oat"; *)
  (* OK *) (* do_roundtr "hashcode.oat"; *)
  (* OK *) (* do_roundtr "tc12.oat"; *)
  (* INVALID *) (* do_roundtr "arrayargs4.oat"; *)
  (* INVALID *) (* do_roundtr "is_prime.oat"; *)
  (* INVALID *) (* do_roundtr "phase2_1.oat"; *)
  (* INVALID *) (* do_roundtr "sqrt.oat"; *)
  (* INVALID *) (* do_roundtr "tc8.oat"; *)
  (* INVALID *) (* do_roundtr "tc_ok4.oat"; *)
  (* INVALID *) (* do_roundtr "toposort.oat"; *)
]

let provided_tests : suite = [
  Test ("student test - rountrip parsing", parsetests);
]
