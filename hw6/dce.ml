(** Dead Code Elimination  *)
open Ll
open Datastructures


(* expose a top-level analysis operation ------------------------------------ *)
(* TASK: This function should optimize a block by removing dead instructions
   - lb: a function from uids to the live-OUT set at the
     corresponding program point
   - ab: the alias map flowing IN to each program point in the block
   - b: the current ll block

   Note:
     Call instructions are never considered to be dead (they might produce
     side effects)

     Store instructions are not dead if the pointer written to is live _or_
     the pointer written to may be aliased.

     Other instructions are dead if the value they compute is not live.

   Hint: Consider using List.filter
 *)
(* Returns true if uid_2 is_live at program point uid *)
let is_live uid lb uid_2 : bool =
  try UidS.mem uid_2 (lb uid) with Not_found -> false

(* Returns true if uid_2 may be aliased at program point uid *)
let may_alias uid ab uid_2 : bool =
  try ((UidM.find uid_2 (ab uid)) = Alias.SymPtr.MayAlias) with Not_found -> false

let is_live_store uid lb ab op : bool =
  match op with
    | Id uid_2 -> is_live uid lb uid_2 || may_alias uid ab uid_2
    | _ -> false

let is_live_insn lb ab (uid, insn) : bool =
  match insn with
    | Call _ -> true
    | Store (_, _, op) -> is_live_store uid lb ab op
    | _ -> is_live uid lb uid

let dce_block (lb:uid -> Liveness.Fact.t)
              (ab:uid -> Alias.fact)
              ({insns; term}:Ll.block) : Ll.block =
  {
    insns = List.filter (is_live_insn lb ab) insns;
    term
  }

let run (lg:Liveness.Graph.t) (ag:Alias.Graph.t) (cfg:Cfg.t) : Cfg.t =

  LblS.fold (fun l cfg ->
    let b = Cfg.block cfg l in

    (* compute liveness at each program point for the block *)
    let lb = Liveness.Graph.uid_out lg l in

    (* compute aliases at each program point for the block *)
    let ab = Alias.Graph.uid_in ag l in

    (* compute optimized block *)
    let b' = dce_block lb ab b in
    Cfg.add_block l b' cfg
  ) (Cfg.nodes cfg) cfg

