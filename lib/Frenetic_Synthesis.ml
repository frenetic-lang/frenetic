open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

(** Utility functions and imports *)
module Compiler = Frenetic_NetKAT_Compiler
module Fabric = Frenetic_Fabric
module Dyad = Fabric.Dyad

let flatten = Frenetic_Util.flatten
let union = Frenetic_NetKAT_Optimize.mk_big_union
let seq   = Frenetic_NetKAT_Optimize.mk_big_seq
let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

(** Essential types *)
exception UnmatchedDyad of Dyad.t

type place  = Fabric.place

(* TODO(basus): This needs to go into a better topology module *)
type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type decider   = topology -> Dyad.t -> Dyad.t -> bool
type chooser   = topology -> Dyad.t -> Dyad.t list -> Dyad.t
type generator = topology -> (Dyad.t * Dyad.t) list -> (policy * policy)

module type SOLVER = sig
  val decide   : decider
  val choose   : chooser
  val generate : generator
end

module type SYNTH = sig
  val synthesize : policy -> policy -> policy -> policy
end

(* Pick n random elements from options *)
let rec random_picks options n =
  let options = Array.of_list options in
  let bound = Array.length options in
  let rec aux n acc =
    if n = 0 then acc
    else
      let index = Random.int bound in
      let acc' = options.(index)::acc in
      aux (n-1) acc' in
  if bound = -1 then [] else aux n []

(* Pick one element at random from options *)
let random_one dyad options= match options with
  | [] -> raise ( UnmatchedDyad dyad )
  | [d] -> d
  | options ->
    let opts = Array.of_list options in
    let index = Random.int (Array.length opts) in
    opts.(index)

(* Use the policy dyads and the corresponding fabric dyads to generate edge
   NetKAT programs that implement the policy dyads atop the fabric dyads. Use a
   unique integer tag per (policy dyad, fabric dyad) to keep them separate. *)
let generate_tagged to_netkat topo pairs =
  let ins, outs, _ = List.fold_left pairs ~init:([],[], 0)
      ~f:(fun (ins, outs, tag) (dyad, pick) ->
          let ins', outs' = to_netkat topo dyad pick tag in
          (ins'::ins, outs'::outs, tag+1)) in
  (union ins, union outs)

(** Topology related functions. Again, need to be replaced by a better topology module. *)
(* Check if the fabric stream and the policy stream start and end at the same,
   or immediately adjacent locations. This is decided using predecessor and
   successor tables. *)
let adjacent topo (src,dst,_,_) fab_stream =
  Fabric.Topo.starts_at topo.preds (fst src) fab_stream &&
  Fabric.Topo.stops_at  topo.succs (fst dst) fab_stream

let go_to topology ((src_sw, src_pt) as src) ((dst_sw, dst_pt) as dst) =
  let pt = if src_sw = dst_sw then dst_pt
  else match Fabric.Topo.precedes topology.preds src dst with
    | Some pt -> pt
    | None ->
      failwith (sprintf "Cannot go to %s from %s in the given topology"
        (Fabric.string_of_place src) (Fabric.string_of_place dst)) in
  Mod (Location (Physical pt))

let come_from topology ((src_sw, src_pt) as src) ((dst_sw, dst_pt) as dst) =
  let pt = if src_sw = dst_sw then src_pt
    else match Fabric.Topo.succeeds topology.succs dst src with
      | Some pt -> pt
      | None ->
      failwith (sprintf "Cannot go to %s from %s in the given topology"
                  (Fabric.string_of_place src) (Fabric.string_of_place dst)) in
  And(Test( Switch dst_sw ),
       Test( Location( Physical pt )))

(** Convert the synthesis types to Z3 program strings. *)
module Z3 = struct

  open Frenetic_Fdd

  exception Inconvertible of string

  type source = Policy | Fabric

  type restraint =
    | Subset
    | Adjacent
    | Tests        of source * Field.t
    | TestsAll     of source * FieldSet.t
    | TestsOnly    of source * FieldSet.t
    | TestsExactly of source * FieldSet.t
    | Not of restraint
    | And of restraint * restraint
    | Or  of restraint * restraint

  let of_int32 i =
    sprintf "#x%08ld" i

  let of_switchId sw =
    (of_int32 (Int64.to_int32_exn sw))

  let of_int64 i =
    sprintf "#x%016Ld" i

  let of_field (f:Field.t) : string = match f with
    | Switch     -> "Switch"
    | Location   -> "Location"
    | VSwitch    -> "VSwitch"
    | VPort      -> "VPort"
    | Vlan       -> "Vlan"
    | VFabric    -> "VFabric"
    | VlanPcp    -> "VlanPcp"
    | EthType    -> "EthType"
    | IPProto    -> "IPProto"
    | EthSrc     -> "EthSrc"
    | EthDst     -> "EthDst"
    | IP4Src     -> "IP4Src"
    | IP4Dst     -> "IP4Dst"
    | TCPSrcPort -> "TCPSrcPort"
    | TCPDstPort -> "TCPDstPort"
    | Channel    -> "Channel"
    | Meta0
    | Meta1
    | Meta2
    | Meta3
    | Meta4      -> "Meta"

  let of_value (v:Value.t) : string =
    try match v with
      | Const k ->
        sprintf "#x%08ld" (Value.to_int32_exn v)
      | Mask(x, 32) ->
        of_int32 (Value.to_int32_exn v)
      | Mask(x, 64) -> of_int64 x
      | _ -> raise (Inconvertible (Value.to_string v))
    with _ -> raise (Inconvertible (Value.to_string v))

  let of_place ((sw,pt):place) : string =
    sprintf "(mk-loc %s %s)"
      (of_switchId sw)
      (of_int32 pt)

  let of_condition ?(name="c0") (c:Fabric.Condition.t) : string * int =
    let length_name = String.tr 'c' 'l' name in
    let const_name = sprintf "(declare-const %s Condition)" name in
    let const_length = sprintf "(declare-const %s Int)" length_name in
    let conds = FieldTable.fold c ~init:[]
        ~f:(fun ~key:field ~data:(pos,negs) acc ->
            let acc' = match pos with
              | Some v -> (true, field, v)::acc
              | None -> acc in
            List.fold negs ~init:acc'
              ~f:(fun acc v -> (false, field, v)::acc)) in
    let length = sprintf "(assert (= %s %d))" length_name (List.length conds) in

    let header = [ length; const_length; const_name ] in
    let _, lines = List.fold conds ~init:(0,header) ~f:(fun (i,acc) (b,f,v) ->
        let store = sprintf "(assert (= %s (store %s %d (mk-cond %B %s %s))))"
            name name i b (of_field f) (of_value v) in
        (i+1, store::acc)) in
    ( String.concat ~sep:"\n" (List.rev lines), (List.length conds) )

  let of_topology ?(name="c0") (t:topology) =
    let rec get_links p acc = match p with
      | Link (s1,p1,s2,p2) -> (s1,p1,s2,p2)::acc
      | Union(p1, p2) -> (get_links p2 (get_links p1 acc))
      | _ -> raise (Inconvertible
                      (sprintf "Cannot serialize this policy as a topology:\n%s\n"
                         (Frenetic_NetKAT_Pretty.string_of_policy p))) in
    let links = get_links t.topo [] in
    let declare = sprintf "(declare-const %s Topology)" name in
    let with_selects = List.fold links ~init:[declare] ~f:(fun acc (s1,p1,s2,p2) ->
        let select =
          sprintf "(assert (= (select %s (mk-loc %s %s)) (mk-loc %s %s)))"
            name (of_switchId s1) (of_int32 p1) (of_switchId s2) (of_int32 p2) in
        select::acc) in
    let with_forall = "(assert (forall ((l1 Location)) (or (or "::with_selects in
    let with_connects = List.fold links ~init:with_forall
        ~f:(fun acc (s1,p1,s2,p2) ->
            let c1 = sprintf "(= l1 (mk-loc %s %s))" (of_switchId s1) (of_int32 p1) in
            let c2 = sprintf "(= l1 (mk-loc %s %s))" (of_switchId s2) (of_int32 p2) in
            c2::c1::acc) in
    let disconnected = sprintf "(= (select %s l1) disconnected)))))" name in
    let lines = disconnected::with_connects in
    String.concat ~sep:"\n" (List.rev lines)

  let of_restraint
      ?(topo="topo") ?(fields="fields")
      ?(fab="cfab") ?(pol="cpol") ?(flen=0) ?(plen=0)
      policy fabric (r:restraint) : string =
    let open Dyad in
    let sname s = match s with
      | Policy -> pol
      | Fabric -> fab in
    let slen s = match s with
      | Policy -> plen
      | Fabric -> flen in
    let mk_asserts asserts fs =
      let declare = sprintf "(declare-const %s (Set Field))" fields in
      let includes = FieldSet.fold fs
          ~init:( declare::asserts ) ~f:(fun acc f ->
              let exists = sprintf "(assert (select %s %s) )" fields (of_field f) in
              exists::acc ) in
      let excludes = List.fold Field.all ~init:includes ~f:(fun acc f ->
          if not ( FieldSet.mem fs f ) then
            (sprintf "(assert (not (select %s %s)))" fields (of_field f))::acc
          else acc) in
      excludes in
    let mk_tests fn src fs asserts =
      (sprintf "(%s %s %d %s)" fn (sname src) (slen src) fields,
       mk_asserts asserts fs) in
    let rec aux r asserts = match r with
      | Subset ->
        sprintf "(subset-of %s %d %s %d)" fab flen pol plen, asserts
      | Adjacent ->
        sprintf "(and (adjacent %s %s %s) (adjacent %s %s %s))"
          topo (of_place (src policy)) (of_place (src fabric))
          topo (of_place (dst policy)) (of_place (dst fabric)),
        asserts
      | Tests (s,f) ->
        sprintf "(tests %s %d %s)" (sname s) (slen s) (of_field f), asserts
      | TestsOnly (s,fs)    -> mk_tests "tests-only"    s fs asserts
      | TestsAll  (s,fs)    -> mk_tests "tests-all"     s fs asserts
      | TestsExactly (s,fs) -> mk_tests "tests-exactly" s fs asserts
      | Not d ->
        let cond, asserts' = (aux d asserts) in
        sprintf "(not %s)" cond, asserts'
      | And(d1, d2) ->
        let cond1, asserts' = aux d1 asserts in
        let cond2, asserts'' = aux d2 asserts' in
        sprintf "(and %s %s)" cond1 cond2, asserts''
      | Or(d1, d2) ->
        let cond1, asserts' = aux d1 asserts in
        let cond2, asserts'' = aux d2 asserts' in
        sprintf "(or %s %s)" cond1 cond2, asserts'' in
    let cond, asserts = aux r [] in
    String.concat ~sep:"\n" (List.rev ( (sprintf "(assert %s)" cond)::asserts) )

  let of_decision restraint topo policy fabric : string =
    let t_z3 = of_topology ~name:"topo" topo in
    let pol_z3, pol_len = of_condition ~name:"cpol" (Dyad.condition policy) in
    let fab_z3, fab_len = of_condition ~name:"cfab" (Dyad.condition fabric) in
    let restraints = of_restraint ~plen:pol_len ~flen:fab_len
        policy fabric restraint in
    let lines = t_z3::pol_z3::fab_z3::restraints::["(check-sat)"] in
    String.concat ~sep:"\n\n" lines

  let mk_decider ?(prereqs_file="z3/prereqs.z3") (r:restraint) : decider =
    let decider restraint topo policy fabric : bool =
      let decision = of_decision restraint topo policy fabric in
      let inc = In_channel.create prereqs_file in
      let prereqs = In_channel.input_all inc in
      In_channel.close inc;
      let outc = Out_channel.create "problem.z3" in
      Out_channel.output_string outc prereqs;
      Out_channel.output_string outc decision;
      Out_channel.close outc;
      let answerc,_ = Unix.open_process "z3 problem.z3" in
      let answer = In_channel.input_all answerc in
      match String.substr_index answer ~pattern:"unsat" with
      | None -> true
      | Some i -> false in
    decider r

end


module Generic : SOLVER = struct
  (** Functions for generating edge NetKAT programs from matched streams **)
  (* Given a policy stream and a fabric stream, generate edge policies to implement *)
  (* the policy stream using the fabric stream *)
  let to_netkat topo
                ((src,dst,cond,actions) as pol)
                ((src',dst',cond',actions') as fab)
                (tag:int): policy * policy =
    let open Fabric.Condition in
    let strip_vlan = 0xffff in
    let satisfy, restore =
      if places_only cond' then
        [ Mod( Vlan tag ) ], [ Mod( Vlan strip_vlan ) ]
      else if is_subset cond' cond then
        let satisfy = satisfy cond' in
        let restore = undo cond' cond in
        (satisfy, restore)
      else
        let mods = satisfy cond' in
        let encapsulate = Mod( Location( Pipe( "encapsulate" ))) in
        let restore = Mod( Location( Pipe( "decapsulate" ))) in
        ( encapsulate::mods , [ restore ]) in
    let to_fabric = go_to topo src src' in
    let to_edge   = Mod( Location (Physical (snd dst))) in
    let in_filter  = Filter (to_pred cond) in
    let out_filter = Filter (come_from topo dst' dst) in
    let modify = Frenetic_Fdd.Action.to_policy actions in
    let ingress = seq ( flatten [
                            [in_filter]; satisfy; [to_fabric] ]) in
    let egress  = seq ( flatten [
                            [out_filter]; restore; [modify]; [to_edge]]) in
    ingress, egress

  (** A fabric stream can carry a policy stream, without encapsulation iff
      1. The endpoints of the two streams are adjacent (see the `adjacent` function)
      and
      2. The fabric stream's conditions only require incoming traffic to enter at
       a certain location only or
      3. The set of fields checked by the fabric stream's conditions are a subset
       of those checked by the policy stream. **)
  let decide topo
      ((src,dst,cond,actions) as pol)
      ((src',dst',cond',actions') as fab) =
    adjacent topo pol fab &&
    ( Fabric.Condition.places_only cond' || Fabric.Condition.is_subset cond' cond)

  (** Just pick one possible fabric stream at random from the set of options *)
  let choose topo dyad options =
    random_one dyad options

  (* Given a list of (policy streams, fabric streams) generate the appropriate
     NetKAT ingress and egress programs *)
  let generate topo pairs : policy * policy =
    generate_tagged to_netkat topo pairs
end

module Optical : SOLVER = struct
  let to_netkat topo
    ((src,dst,cond,actions)) ((src',dst',cond',actions'))
    (tag:int): policy * policy =
  let open Fabric.Condition in
  let strip_vlan = 0xffff in
  let to_fabric = go_to topo src src' in
  let to_edge   = Mod( Location (Physical (snd dst))) in
  let in_filter  = Filter (to_pred cond) in
  let out_filter = Filter (come_from topo dst' dst) in
  let modify = Frenetic_Fdd.Action.to_policy actions in
  let ingress = seq ([ in_filter; Mod( Vlan tag ); to_fabric ]) in
  let egress  = seq ([ out_filter; Mod( Vlan strip_vlan ); modify; to_edge ]) in
  ingress, egress

  let decide topo ((_,_,cond,_) as pol) ((_,_,cond',_) as fab) =
    adjacent topo pol fab && Fabric.Condition.places_only cond'

  (** Just pick one possible fabric stream at random from the set of options *)
  let choose topo dyad options =
    random_one dyad options

  let generate topo pairs : policy * policy =
    generate_tagged to_netkat topo pairs

end

module Make (S:SOLVER) = struct
  open S

  (** Core matching function *)
  let matching topology (from_policy:Dyad.t list) (from_fabric:Dyad.t list)
    : policy * policy =

    (* For each policy stream, find the set of fabric streams that could be used
       to carry it. This is done using the `decide` function from the SOLVER
       that allows the caller to specify what criteria are important, perhaps
       according to the properties of the fabric. *)
    let partitions = List.fold_left from_policy ~init:[]
        ~f:(fun acc stream ->
            let streams = List.filter from_fabric ~f:(decide topology stream) in
            let partition = (stream, streams) in
            (partition::acc)) in

    (* Pick a smaller set of fabric streams to actually carry the policy streams *)
    let pairs =
      List.fold_left partitions ~init:[] ~f:(fun acc (stream, opts) ->
          let pick = choose topology stream opts in
          (stream, pick)::acc) in

    generate topology pairs

  let synthesize (policy:policy) (fabric:policy) (topo:policy) : policy =
    (* Streams are condition/modification pairs with empty actioned pairs filtered out *)
    let policy_streams = Fabric.Dyad.of_policy policy in
    let fabric_streams = Fabric.Dyad.of_policy fabric in
    let preds = Fabric.Topo.predecessors topo in
    let succs = Fabric.Topo.successors topo in
    let topology = {topo; preds; succs} in
    let ingress, egress = matching topology
        policy_streams fabric_streams in
    Union(ingress, egress)

end
