open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"


  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let apply_bop bop c1 c2 : int64 =
  let open Int64 in
  match bop with
    | Add -> add c1 c2
    | Sub -> sub c1 c2
    | Mul -> mul c1 c2
    | Shl -> shift_left c1 @@ to_int c2
    | Lshr -> shift_right_logical c1 @@ to_int c2
    | Ashr -> shift_right c1 @@ to_int c2
    | And -> logand c1 c2
    | Or -> logor c1 c2
    | Xor -> logxor c1 c2

let bop_result bop sym1 sym2 : SymConst.t =
  let open SymConst in
  match sym1 with
    | UndefConst -> UndefConst
    | NonConst -> NonConst
    | Const c1 ->
      begin match sym2 with
        | UndefConst -> UndefConst
        | NonConst -> NonConst
        | Const c2 -> Const (apply_bop bop c1 c2)
      end

let apply_icmp cnd c1 c2 : int64 =
  let op = match cnd with
    | Eq -> (=)
    | Ne -> (<>)
    | Slt -> (<)
    | Sle -> (<=)
    | Sgt -> (>)
    | Sge -> (>=)
  in
  if op c1 c2 then 1L else 0L

let icmp_result cnd sym1 sym2 : SymConst.t =
  let open SymConst in
  match sym1 with
    | UndefConst -> UndefConst
    | NonConst -> NonConst
    | Const c1 ->
      begin match sym2 with
        | UndefConst -> UndefConst
        | NonConst -> NonConst
        | Const c2 -> Const (apply_icmp cnd c1 c2)
      end

let op_sym (op: operand) (d:fact) : SymConst.t =
  let open SymConst in
  match op with
    | Const c -> Const c
    | Id uid -> UidM.find_or NonConst d uid
    | _ -> NonConst

let insn_sym ((uid, insn):uid * insn) (d:fact) : SymConst.t =
  let open SymConst in
  match insn with
    | Binop (bop, _, op1, op2) -> bop_result bop (op_sym op1 d) (op_sym op2 d)
    | Icmp (cnd, _, op1, op2) -> icmp_result cnd (op_sym op1 d) (op_sym op2 d)
    | Store _ -> UndefConst
    | Call (Void, _, _) -> UndefConst
    | _ -> NonConst

let insn_flow ((uid, insn):uid * insn) (d:fact) : fact =
  let sym = insn_sym (uid, insn) d in
  UidM.add uid sym d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  =
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let join_symconst (a: SymConst.t) (b: SymConst.t) : SymConst.t =
      let open SymConst in
      match a with
      | UndefConst -> a
      | NonConst -> if b = UndefConst then b else a
      | Const ac -> begin match b with
          | Const bc -> if ac = bc then Const ac else NonConst
          | _ -> b
        end
  
    let merge (k: UidM.key) (a: SymConst.t option) (b: SymConst.t option) : SymConst.t option  =
      match a with
        | None -> b
        | Some asym -> begin match b with
            | None -> a
            | Some bsym -> Some (join_symconst asym bsym)
          end

    let combine (ds:fact list) : fact =
      List.fold_left (UidM.merge merge) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper
   functions.                                                                 *)

(* replace uids with const op *)
let replace_op (fact: Fact.t) (op:Ll.operand) : Ll.operand =
  match op with
    | Id uid -> begin match UidM.find uid fact with
        | SymConst.Const c -> Ll.Const c
        | _ -> Id uid
      end
    | op -> op

let replace_ops (fact: Fact.t) (ops: Ll.operand list) : Ll.operand list =
  List.map (replace_op fact) ops

(* replace uids that hold constant values *)
let cp_insn (cb: uid -> Fact.t) ((uid, insn): Ll.uid * Ll.insn) : (Ll.uid * Ll.insn) =
  let fact = cb uid in
  let f = replace_op fact in
  let g = replace_ops fact in
  let i = match insn with
    | Binop (bop, ty, op1, op2) -> Binop (bop, ty, f op1, f op2)
    | Alloca ty -> Alloca ty
    | Load (ty, op) -> Load (ty, f op)
    | Store (ty, op1, op2) -> Store (ty, f op1, f op2)
    | Icmp (cnd, ty, op1, op2) -> Icmp (cnd, ty, f op1, f op2)
    | Bitcast (ty, op, ty2) -> Bitcast (ty, f op, ty2)
    | Gep (ty, op, oplist) -> Gep (ty, f op, g oplist)
    | Call (ty, op, tyoplist) ->
      let (tys, ops) = List.split tyoplist in
      let new_tyoplist = List.combine tys (g ops) in
      Call (ty, f op, new_tyoplist)
  in
  (uid, i)

let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in


  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in

    let {insns; term} = b in

    let new_insns = List.map (cp_insn cb) insns in
    let (uid, t) = term in
    let f = replace_op (cb uid) in
    let new_term = match t with
      | Ret (ty, None) -> Ret (ty, None)
      | Ret (ty, Some op) -> Ret (ty, Some (f op))
      | Br lbl -> Br lbl
      | Cbr (op, lbl1, lbl2) -> Cbr (f op, lbl1, lbl2)
    in

    let b' = { insns = new_insns; term = (uid, new_term) } in
    Cfg.add_block l b' cfg
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
