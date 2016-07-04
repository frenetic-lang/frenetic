open Core.Std
open Frenetic_Fdd
open Frenetic_NetKAT

module Compiler = Frenetic_NetKAT_Compiler
module FieldTable = Hashtbl.Make(Field)

type condition = (Value.t option * Value.t list) FieldTable.t
type place     = (switchId * portId)
type stream    = place * place * condition * Action.t

(** Import and rename utility functions *)
let conjoin = Frenetic_NetKAT_Optimize.mk_big_and
let disjoin = Frenetic_NetKAT_Optimize.mk_big_or
let seq     = Frenetic_NetKAT_Optimize.mk_big_seq
let union   = Frenetic_NetKAT_Optimize.mk_big_union
let strip_vlan = 0xffff

let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

(** Exceptions **)
exception IncompletePlace of string
exception NonFilterNode of policy
exception ClashException of string
exception CorrelationException of string

(** Various conversations between types **)
let pred_of_place ((sw,pt):place) =
  And( Test( Switch sw ), Test( Location( Physical pt )))

let pred_of_condition (c:condition) : pred =
  let preds = FieldTable.fold c ~init:[]
      ~f:(fun ~key:field ~data:(pos, negs) acc ->
          let negs = List.map negs ~f:(fun v ->
              Neg (Pattern.to_pred (field, v))) in
          match pos with
            | None -> (conjoin negs)::acc
            | Some v ->
              let p = And(conjoin negs, Pattern.to_pred (field, v)) in p::acc) in
  conjoin preds

let place_of_options sw pt = match sw, pt with
  | Some sw, Some pt -> (sw, pt)
  | None   , Some pt -> raise (IncompletePlace "No switch specified")
  | Some sw, None    -> raise (IncompletePlace "No port specified")
  | None   , None    -> raise (IncompletePlace "No switch and port specified")


(** String conversions and pretty printing **)
let string_of_pred = Frenetic_NetKAT_Pretty.string_of_pred
let string_of_policy = Frenetic_NetKAT_Pretty.string_of_policy

let string_of_place ((sw,pt):place) =
  sprintf "(%Ld:%ld)" sw pt

(* TODO(basus): remove conversions to policies *)
let string_of_stream (s:stream) : string =
  let (sw_in, pt_in), (sw_out, pt_out), condition, action = s in
  sprintf
    "From: Switch:%Ld Port:%ld\nTo Switch:%Ld Port:%ld\nCondition: %s\nAction: %s\n"
    sw_in pt_in sw_out pt_out
    (string_of_pred (pred_of_condition condition))
    (string_of_policy (Action.to_policy action))


(** Iterate through a policy and translates Links to matches on source and *)
(** modifications to the destination *)
let rec dedup (pol:policy) : policy =
  let open Frenetic_NetKAT in
  let at_place sw pt = Filter (pred_of_place (sw,pt)) in
  let to_place sw pt =
    let sw_mod = Mod (Switch sw) in
    let pt_mod = Mod (Location (Physical pt)) in
    Seq ( sw_mod, pt_mod ) in
  match pol with
  | Filter a    -> Filter a
  | Mod hv      -> Mod hv
  | Union (p,q) -> Union(dedup p, dedup q)
  | Seq (p,q)   -> Seq(dedup p, dedup q)
  | Star p      -> Star(dedup p)
  | Link (s1,p1,s2,p2) ->
    Seq (at_place s1 p1, to_place s2 p2)
  | VLink _ -> failwith "Fabric: Cannot remove Dups from a policy with VLink"

let fuse field (pos, negs) (pos', negs') =
  let pos = match pos, pos' with
    | None, None   -> None
    | None, Some v
    | Some v, None -> Some v
    | Some v1, Some v2 ->
      let msg = sprintf "Field (%s) expected to have clashing values of (%s) and (%s) "
          (Field.to_string field) (Value.to_string v1) (Value.to_string v2) in
      raise (ClashException msg) in
  let negs = match negs, negs' with
    | [], [] -> []
    | vs, [] -> vs
    | [], vs -> vs
    | vs1, vs2 -> List.unordered_append vs1 vs2 in
  (pos, negs)

(* Given a policy, guard it with the ingresses, sequence and iterate with the *)
(* topology and guard with the egresses, i.e. form in;(p.t)*;p;out *)
let assemble (pol:policy) (topo:policy) ings egs : policy =
  let to_filter (sw,pt) = Filter( And( Test(Switch sw),
                                       Test(Location (Physical pt)))) in
  let ingresses = union (List.map ings ~f:to_filter) in
  let egresses  = union (List.map egs ~f:to_filter) in
  seq [ ingresses;
        Star(Seq(pol, topo)); pol;
        egresses ]

(* TODO(basus): This only keep the last destination, but there might be many *)
let destination_of_action (action:Action.t) : (switchId option * portId option * Action.t) =
  let open Frenetic_NetKAT in
  Action.Par.fold action ~init:(None, None, Action.Par.empty)
      ~f:(fun (sw,pt,acc) seq ->
          let sw, pt, seq' = Action.Seq.fold_fields seq ~init:(sw,pt,Action.Seq.empty)
              ~f:(fun ~key ~data (sw,pt,acc) ->
                  match Pattern.to_hv (key, data) with
                  | IP4Src(nwAddr, 32l) ->
                    (sw,pt, Action.Seq.add acc (Action.F key) data)
                  | IP4Dst(nwAddr, 32l) ->
                    (sw,pt, Action.Seq.add acc (Action.F key) data)
                  | Switch sw ->
                    (Some sw, pt, acc)
                  | Location(Physical pt) ->
                    (sw, Some pt, acc)
                  | hv ->
                    (sw,pt, Action.Seq.add acc (Action.F key) data)) in
          (sw, pt, Action.Par.add acc seq'))

let stream_of_fdd_path action headers =
  let update sw pt tbl field pos negs = match field, pos with
    | Field.Switch, Some( Value.Const sw' ) -> begin match sw with
        | None -> (Some sw', pt)
        | Some sw ->
          let msg = sprintf "Switch field has clashing values of %Ld and %Ld" sw sw' in
          raise (ClashException msg) end
    | Field.Location, Some( Value.Const pt' as p) -> begin match pt with
        | None -> (sw, Some(Value.to_int32_exn p))
        | Some pt ->
          let msg = sprintf "Port field has clashing values of %ld and %ld" pt
              (Value.to_int32_exn p) in
          raise (ClashException msg) end
    | _ ->
      Hashtbl.Poly.update tbl field ~f:(function
          | None -> ( pos, negs )
          | Some c -> fuse field (pos,negs) c);
      (sw, pt) in
  let tbl = FieldTable.create () in
  let src_sw, src_pt = List.fold_left headers ~init:(None, None)
      ~f:(fun (sw,pt) (f, p, ns) ->
          update sw pt tbl f p ns) in
  let dst_sw, dst_pt, action = destination_of_action action in
  (place_of_options src_sw src_pt, place_of_options dst_sw dst_pt,
   tbl, action)

let streams_of_policy (pol:policy) : stream list =
  let rec traverse node path =
    match FDD.unget node with
    | FDD.Branch ((v,l), t, f) ->
      let true_pred   = (v, Some l, []) in
      let true_paths  = traverse t ( true_pred::path ) in
      let false_pred  = (v, None, [l]) in
      let false_paths = traverse f ( false_pred::path ) in
      List.unordered_append true_paths false_paths
    | FDD.Leaf r ->
      if Action.is_zero r then [] else [ (r, path) ] in

  let deduped = dedup pol in
  let fdd = compile_local deduped in
  let paths = traverse fdd [] in
  List.map paths ~f:(fun (a, hs) -> stream_of_fdd_path a hs)

(** Functions for finding adjacent nodes in a given topology *)
let find_predecessors (topo:policy) =
  let open Frenetic_NetKAT in
  let table = Hashtbl.Poly.create () in
  let rec populate pol = match pol with
    | Union(p1, p2) ->
      populate p1;
      populate p2
    | Link (s1,p1,s2,p2) ->
      Hashtbl.Poly.add_exn table (s2,p2) (s1,p1);
    | p -> failwith (sprintf "Unexpected construct in policy: %s\n"
                       (Frenetic_NetKAT_Pretty.string_of_policy p)) in
  populate topo;
  table

let find_successors (topo:policy) =
  let open Frenetic_NetKAT in
  let table = Hashtbl.Poly.create () in
  let rec populate pol = match pol with
    | Union(p1, p2) ->
      populate p1;
      populate p2
    | Link (s1,p1,s2,p2) ->
      Hashtbl.Poly.add_exn table (s1,p1) (s2,p2);
    | p -> failwith (sprintf "Unexpected construct in policy: %s\n"
                       (Frenetic_NetKAT_Pretty.string_of_policy p)) in
  populate topo;
  table

(* Does (sw,pt) precede (sw',pt') in the topology, using the predecessor table *)
let precedes tbl (sw,_) (sw',pt') =
  match Hashtbl.Poly.find tbl (sw',pt') with
  | Some (pre_sw,pre_pt) -> if pre_sw = sw then Some pre_pt else None
  | None -> None

(* Does (sw,pt) succeed (sw',pt') in the topology, using the successor table *)
let succeeds tbl (sw,_) (sw',pt') =
  match Hashtbl.Poly.find tbl (sw',pt') with
  | Some (post_sw, post_pt) -> if post_sw = sw then Some post_pt else None
  | None -> None


(** Start graph-based retargeting. *)

module OverNode = struct
  type t = (switchId * portId)

  let default = (0L, 0l)

  let compare l1 l2 = Pervasives.compare l1 l2
  let hash l1       = Hashtbl.hash l1

  let equal (sw,pt) (sw',pt') =
    sw = sw' && pt = pt'

  let to_string (sw,pt) = sprintf "(%Ld:%ld)" sw pt
end

module OverEdge = struct
  open Frenetic_NetKAT

  type t =
    | Internal                            (* Between ports on the same switch *)
    | Exact of condition * Action.t       (* The fabric delivers between those exact point *)
    | Adjacent of condition * Action.t    (* Fabric delivers between locations attached to
                                             these in the topology *)

  let default = Internal

  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module Overlay = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (OverNode)
      (OverEdge)
  include G
end

module Weight = struct
  type t = int
  type edge = Overlay.E.t
  let weight e = 1
  let compare = Int.compare
  let add = (+)
  let zero = 0
end

module OverPath = Graph.Path.Dijkstra(Overlay)(Weight)

(* Add edges between ports of the same switch *)
let switch_inter_connect g =
  let open OverEdge in
  Overlay.fold_vertex (fun (sw,pt) g ->
      Overlay.fold_vertex (fun (sw',pt') g ->
          if sw = sw' && not (pt = pt')
          then Overlay.add_edge_e g ((sw,pt), Internal, (sw,pt'))
          else g ) g g) g g

let retarget (policy:stream list) (fabric:stream list) (topo:policy) =
  let open Frenetic_NetKAT in
  let preds = find_predecessors topo in
  let succs = find_successors topo in

  (* Add vertices for all policy locations *)
  let graph = List.fold policy ~init:Overlay.empty
      ~f:(fun g (src, sink, _, _) ->
          let g' = Overlay.add_vertex g src in
          Overlay.add_vertex g' sink) in

  (* Add edges between all location pairs reachable via the fabric *)
  let graph = ( List.fold fabric ~init:graph
      ~f:(fun g (src, sink, condition, action) ->
           let open OverEdge in
           let g' = Overlay.add_edge_e g (src, Exact(condition,action) , sink) in
           let src' = Hashtbl.Poly.find preds src in
           let sink' = Hashtbl.Poly.find succs sink in
           match src', sink' with
           | Some src', Some sink' ->
            Overlay.add_edge_e g' (src', Adjacent(condition, action), sink')
           | _ -> g'
         ) ) in

  let graph = switch_inter_connect graph in

  (* Given a list of hops consisting of alternating fabric paths and internal
     forwards on edge switches, generate ingress, egress, or bounce rules to
     stitch the hops together, forming a VLAN-tagged path between the naive
     policy endpoints *)
  let rec stitch src sink path tag condition action =
    let open OverEdge in
    match path with
    | [] -> ([], [])
    | (_,Internal,(_,in_pt))::rest ->
      let start = pred_of_place src in
      let cond = pred_of_condition condition in
      let filter = Filter( And( start, cond )) in
      let ingress = seq [ filter;
                          Mod( Vlan tag );
                          Mod( Location( Physical in_pt)) ] in
      let ins, outs = stitch src sink rest tag condition action in
      (ingress::ins, outs)
    | (_,Adjacent _,_)::((out_sw,out_pt),Internal,(_,in_pt))::rest ->
      let test_loc = And( Test( Switch out_sw ),
                          Test( Location( Physical out_pt ))) in
      let filter = Filter( And( Test( Vlan tag ),
                                test_loc )) in
      let egress = begin match rest with
        | [] ->
          let mods = Action.to_policy action in
          let out = Mod( Location( Physical (snd sink) )) in
          seq [ filter; Mod( Vlan strip_vlan); Seq(mods, out)]
        | rest ->
          seq [ filter; Mod( Location( Physical in_pt )) ] end in
      let ins, outs = stitch src sink rest tag condition action in
      (ins, egress::outs)
    | _ -> failwith "Malformed path" in

  let ingresses, egresses, _ = List.fold policy ~init:([],[],1)
      ~f:(fun (ins, outs, tag) (src,sink,condition,action) ->
          try
            let path,_ = OverPath.shortest_path graph src sink in
            let ingress, egress = stitch src sink path tag condition action in
            ( List.rev_append ingress ins,
              List.rev_append egress  outs,
              tag + 1 )
          with Not_found ->
            printf "No path between %s and %s\n%!"
              (string_of_place src) (string_of_place sink);
            (ins,outs, tag)) in

  ingresses, egresses
