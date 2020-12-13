(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Pervasives.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"

  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis.

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     may be aliased
   - Other instructions do not define pointers

 *)
let op_to_uid op =
  match op with
    | Id uid -> Some uid
    | _ -> None

let add_unique uid d =
  UidM.add uid SymPtr.Unique d

let add_alias uid d =
  UidM.add uid SymPtr.MayAlias d

let update_alias uid d =
  let update = fun i -> if i = SymPtr.UndefAlias then i else SymPtr.MayAlias in
  try UidM.update update uid d with Not_found -> d

let update_op op d =
  match op with
    | Id uid -> update_alias uid d
    | _ -> d

let update_aliases uids d =
  let f = fun a b -> update_alias b a in
  List.fold_left f d uids

let update_ops ops d =
  let uids = List.filter_map op_to_uid ops in
  update_aliases uids d

let add_alias_if_ptr uid ty d =
  match ty with
    | Ptr _ -> add_alias uid d
    | _ -> d

let add_alias_if_ptr_ptr uid ty d =
  match ty with
    | Ptr (Ptr _) -> add_alias uid d
    | _ -> d

let insn_flow ((uid, insn):uid * insn) (d:fact) : fact =
  match insn with
    | Alloca _ ->             d |> add_unique uid
    | Load (ty, _) ->         d |> add_alias_if_ptr_ptr uid ty
    | Call (ty, op, args) ->  d |> add_alias_if_ptr uid ty |> update_ops (List.map snd args)
    | Bitcast (ty, op, _) ->  d |> add_alias_if_ptr uid ty |> update_op op
    | Gep (_, op, opl) ->     d |> add_alias uid |> update_ops (op::opl)
    | Store (_, op, _) ->     d |> update_op op
    | _ ->                    d

(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int =
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* TASK: complete the "combine" operation for alias analysis.

       The alias analysis should take the join over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful.

       It may be useful to define a helper function that knows how to take the
       join of two SymPtr.t facts.
    *)
    let join_symptr (a: SymPtr.t) (b: SymPtr.t) : SymPtr.t =
      match a with
        | MayAlias -> a
        | Unique -> if b = SymPtr.MayAlias then b else a
        | UndefAlias -> b

    let merge (k: UidM.key) (a: SymPtr.t option) (b: SymPtr.t option) : SymPtr.t option =
      match a with
        | Some ap -> begin match b with
            | Some bp -> Some (join_symptr ap bp)
            | None -> a
          end
        | None -> b

    let combine (ds:fact list) : fact =
      List.fold_left (UidM.merge merge) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter
     to the function may be aliased *)
  let alias_in =
    List.fold_right
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m)
      g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg

