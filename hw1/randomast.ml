open Hellocaml

let _ = Random.self_init ()

let rnd_tnum i : int = Random.int i

let rnd_chr () : string = String.make 1 @@ Char.chr (97 + (Random.int 26))

(* weighted, so we have more 0s, 1s and -1s *)
let rnd_lit () : int64 = match Random.int64 100L with
  | i when i < 15L -> -1L
  | i when i < 30L -> 0L
  | i when i < 45L -> 1L
  | i -> i

let rnd_ast () : exp =
  let rec rnd_ast_h acc = let bnd = begin
    match acc with
      | i when i < 3 -> 3
      | i when i < 100 -> 5
      | _ -> 1
    end in if bnd = 1 then Const(rnd_lit ()) else
      match rnd_tnum bnd with
        | 0 -> Add(rnd_ast_h (acc+1), rnd_ast_h (acc+1))
        | 1 -> Mult(rnd_ast_h (acc+1), rnd_ast_h (acc+1))
        | 2 -> Neg(rnd_ast_h (acc+1))
        | 3 -> Var(rnd_chr ())
        | 4 -> Const(rnd_lit ())
  in rnd_ast_h 0

let rnd_ctx (s : string list) : ctxt =
  List.map (fun x -> (x, rnd_lit ())) s

let print_vars ast = print_string @@
  "VARS: " ^ (String.concat " " (vars_of ast)) ^ "\n"

let print_ctx (c:ctxt) = print_string @@
  "CTX: "
  ^ (String.concat " "
    (List.map
      (fun x -> "(" ^ (fst x) ^ "," ^ (Int64.to_string @@ snd x) ^ ")")
      c)
    )
  ^ "\n"

let print_info ((c:ctxt), (e:exp))   =
  begin
    print_string ("AST: " ^ (string_of e) ^ "\n");
    print_vars e;
    print_ctx c
  end


let rnd_ctx_exp_pair () : (ctxt * exp) =
  let ast = rnd_ast () in
  (rnd_ctx (vars_of ast), ast)

let rnd_ctx_exp_pair_p (i:int) : (ctxt * exp) = begin
    let (c, e) = rnd_ctx_exp_pair () in
      print_string @@ "test case " ^ (string_of_int i) ^ "\n";
      print_info (c, e);
      (c, e)
  end

(* [IPushC 2L; IPushC 3L; IMul] *)
let dump_stack (p:program) : unit =
  let s_tf (ins:insn) = match ins with
    | IPushC(i) -> "IPushC " ^ (Int64.to_string i)
    | IPushV(s) -> "IPushV " ^ s
    | IMul -> "IMul"
    | IAdd -> "IAdd"
    | INeg -> "INeg"
  in
    print_string @@
      "STACK: [" ^ (String.concat "; " (List.map s_tf p)) ^ "]\n"
