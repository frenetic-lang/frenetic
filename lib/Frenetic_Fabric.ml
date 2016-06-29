open Core.Std
open Frenetic_Fdd
open Frenetic_Network
open Frenetic_OpenFlow

module Compiler = Frenetic_NetKAT_Compiler

type pred   = Frenetic_NetKAT.pred
type policy = Frenetic_NetKAT.policy
type fabric = (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t

type condition  = Field.t * Value.t option * Value.t list
type stream     = condition list * Action.t
type loc        = (switchId * portId)
type loc_stream = ( loc * loc * stream )

exception NonFilterNode of policy
exception ClashException of string
exception CorrelationException of string

(** Utility functions *)
let conjoin = Frenetic_NetKAT_Optimize.mk_big_and

let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

let pred_of_cond ((f,pos,neg):condition) : pred =
  let pos' = match pos with
    | None -> []
    | Some v -> [ Frenetic_Fdd.Pattern.to_pred (f, v) ] in
  let negs = List.fold_left neg ~init:pos'
      ~f:(fun acc v -> (Neg (Frenetic_Fdd.Pattern.to_pred (f, v)))::acc) in
  conjoin negs

let pred_of_conds (conds: condition list) =
  let open Frenetic_NetKAT in
  conjoin (List.map conds ~f:pred_of_cond)

let string_of_loc ((sw,pt):loc) =
  sprintf "(%Ld:%ld)" sw pt

let string_of_stream (conds, act) =
  sprintf "Condition: %s\nAction: %s\n"
    (Frenetic_NetKAT_Pretty.string_of_pred (pred_of_conds conds))
    (Frenetic_NetKAT_Pretty.string_of_policy (Action.to_policy act))

let string_of_located_stream ((sw,pt),(sw',pt'),stream) =
  let src = sprintf "Source: Switch: %Ld Port:%ld" sw pt in
  let sink = sprintf "Sink: Switch: %Ld Port:%ld" sw' pt' in
  let stream = string_of_stream stream in
  String.concat ~sep:"\n" [src; sink; stream]

let string_of_pred   = Frenetic_NetKAT_Pretty.string_of_pred
let string_of_policy = Frenetic_NetKAT_Pretty.string_of_policy

let seq   = Frenetic_NetKAT_Optimize.mk_big_seq
let union = Frenetic_NetKAT_Optimize.mk_big_union


(** Fabric generators, from a topology or policy *)

let strip_vlan = 0xffff
let mk_flow (pat:Pattern.t) (actions:group) : flow =
  { pattern = pat
  ; action = actions
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  }

let drop = mk_flow Pattern.match_all [[[]]]

let vlan_per_port (net:Net.Topology.t) : fabric =
  let open Net.Topology in
  let tags = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  iter_edges (fun edge ->
      let src, port = edge_src edge in
      let label = vertex_to_label net src in
      let pattern = { Pattern.match_all with dlVlan =
                                               Some (Int32.to_int_exn port)} in
      let actions = [ [ [ Modify(SetVlan (Some strip_vlan)); Output (Physical port) ] ] ] in
      let flow = mk_flow pattern actions in
      match Node.device label with
      | Node.Switch ->
        Hashtbl.Poly.change tags (Node.id label)
          ~f:(fun table -> match table with
              | Some flows -> Some( flow::flows )
              | None -> Some [flow; drop] )
      | _ -> ()) net;
  tags

let shortest_path (net:Net.Topology.t)
    (ingress:switchId list) (egress:switchId list) : fabric =
  let open Net.Topology in
  let vertexes = vertexes net in
  let vertex_from_id swid =
    let vopt = VertexSet.find vertexes (fun v ->
      (Node.id (vertex_to_label net v)) = swid) in
    match vopt with
    | Some v -> v
    | None -> failwith (Printf.sprintf "No vertex for switch id: %Ld" swid )
  in

  let mk_flow_mod (tag:int) (port:int32) : flow =
    let pattern = { Pattern.match_all with dlVlan = Some tag } in
    let actions = [[[ Output (Physical port) ]]] in
    mk_flow pattern actions
  in


  let table = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  let tag = ref 10 in
  List.iter ingress ~f:(fun swin ->
    let src = vertex_from_id swin in
    List.iter egress ~f:(fun swout ->
      if swin = swout then ()
      else
        let dst = vertex_from_id swout in
        tag := !tag + 1;
        match Net.UnitPath.shortest_path net src dst with
        | None -> ()
        | Some p ->
          List.iter p ~f:(fun edge ->
            let src, port = edge_src edge in
            let label = vertex_to_label net src in
            let flow_mod = mk_flow_mod !tag port in
            match Node.device label with
            | Node.Switch ->
              Hashtbl.Poly.change table (Node.id label)
                ~f:(fun table -> match table with
                | Some flow_mods -> Some( flow_mod::flow_mods )
                | None -> Some [flow_mod; drop] )
            | _ -> ())));
  table

let of_local_policy (pol:policy) (sws:switchId list) : fabric =
  let fabric = Hashtbl.Poly.create ~size:(List.length sws) () in
  let compiled = compile_local pol in
  List.iter sws ~f:(fun swid ->
      let table = (Compiler.to_table swid compiled) in
      match Hashtbl.Poly.add fabric ~key:swid ~data:table with
      | `Ok -> ()
      | `Duplicate -> printf "Duplicate table for switch %Ld\n" swid
    ) ;
  fabric


let of_global_policy (pol:policy) (sws:switchId list) : fabric =
  let fabric = Hashtbl.Poly.create ~size:(List.length sws) () in
  let compiled = Compiler.compile_global pol in
  List.iter sws ~f:(fun swid ->
      let table = (Compiler.to_table swid compiled) in
      match Hashtbl.Poly.add fabric ~key:swid ~data:table with
      | `Ok -> ()
      | `Duplicate -> printf "Duplicate table for switch %Ld\n" swid
    ) ;
  fabric

let to_string (fab:fabric) : string =
  let buf = Buffer.create (Hashtbl.length fab * 100) in
  Hashtbl.Poly.iteri fab ~f:(fun ~key:swid ~data:mods ->
      Buffer.add_string buf (
        Frenetic_OpenFlow.string_of_flowTable
          ~label:(sprintf "Switch %Ld |\n" swid)
          mods)) ;
  Buffer.contents buf


(** Start of retargeting compiler code *)

(** Iterate through a policy and remove any modifications to the switch field. *)
let rec remove_switch_mods (pol:policy) : policy =
  let open Frenetic_NetKAT in
  match pol with
  | Union (p, Mod(Switch _)) -> p
  | Union (Mod(Switch _), p) -> p
  | Seq (p, Mod(Switch _))   -> p
  | Seq (Mod(Switch _), p)   -> p
  | Star p                   -> Star (remove_switch_mods p)
  | p                        -> p

(** Iterate through a policy and translates Links to matches on source and *)
(** modifications to the destination *)
let rec remove_dups (pol:policy) : policy =
  let open Frenetic_NetKAT in
  let at_location sw pt =
    let sw_test = Test (Switch sw) in
    let pt_test = Test (Location (Physical pt)) in
    let loc_test = Frenetic_NetKAT_Optimize.mk_and sw_test pt_test in
    Filter loc_test in
  let to_location sw pt =
    let sw_mod = Mod (Switch sw) in
    let pt_mod = Mod (Location (Physical pt)) in
    Seq ( sw_mod, pt_mod ) in
  match pol with
  | Filter a    -> Filter a
  | Mod hv      -> Mod hv
  | Union (p,q) -> Union(remove_dups p, remove_dups q)
  | Seq (p,q)   -> Seq(remove_dups p, remove_dups q)
  | Star p      -> Star(remove_dups p)
  | Link (s1,p1,s2,p2) ->
    Seq (at_location s1 p1, to_location s2 p2)
  | VLink _ -> failwith "Fabric: Cannot remove Dups from a policy with VLink"

(** Extract the alpha and beta pair from a policy. The alpha is the predicate *)
(** for the policy, and the beta is the modification. Alpha corresponds to the
    internal nodes in the FDD and the beta to the leaves *)
let extract (pol:policy) : stream list =
  let rec get_paths node path =
    match FDD.unget node with
    | FDD.Branch ((v,l), t, f) ->
      let true_pred   = (v, Some l, []) in
      let true_paths  = get_paths t ( true_pred::path ) in
      let false_pred  = (v, None, [l]) in
      let false_paths = get_paths f ( false_pred::path ) in
      List.unordered_append true_paths false_paths
    | FDD.Leaf r ->
      [ (r, path) ] in

  let partition (action, conds) : stream =
    let open Frenetic_Fdd in
    let tbl = Hashtbl.Poly.create ~size:(List.length conds) () in
    let fuse f (pos,neg) (pos',neg') =
      let pos = match pos, pos' with
        | None  , None     -> None
        | Some v, None     -> Some v
        | None  , Some v   -> Some v
        | Some v1, Some v2 ->
          let msg = sprintf "Field (%s) expected to have clashing values of (%s) and (%s) "
              (Field.to_string f) (Value.to_string v1) (Value.to_string v2) in
          raise (ClashException msg) in
      let neg = match neg, neg' with
        | [], [] -> []
        | vs, [] -> vs
        | [], vs -> vs
        | vs1, vs2 -> List.unordered_append vs1 vs2 in
      (pos, neg) in
    List.iter conds ~f:(fun (field,pos,neg) ->
        Hashtbl.Poly.update tbl field ~f:(function
            | None -> ( pos,neg )
            | Some c -> fuse field (pos,neg) c));
    let branches = Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:field ~data:(pos,neg) acc -> (field, pos, neg)::acc) in
    (branches, action) in

  let deduped = remove_dups pol in
  let fdd     = compile_local deduped in
  let paths   = get_paths fdd [] in
  List.map paths ~f:partition

(* Given a policy, guard it with the ingresses, sequence and iterate with the *)
(* topology and guard with the egresses, i.e. form in;(p.t)*;p;out *)
let assemble (pol:policy) (topo:policy) ings egs : policy =
  let open Frenetic_NetKAT in
  let to_filter (sw,pt) = Filter( And( Test(Switch sw),
                                       Test(Location (Physical pt)))) in
  let ingresses = union (List.map ings ~f:to_filter) in
  let egresses  = union (List.map egs ~f:to_filter) in
  seq [ ingresses;
        Star(Seq(pol, topo)); pol;
        egresses ]

(** Functions for finding adjacent nodes in a given topology *)
let find_predecessors (topo:policy) =
  let open Frenetic_NetKAT in
  let switch_table = Hashtbl.Poly.create () in
  let loc_table = Hashtbl.Poly.create () in
  let rec populate pol = match pol with
    | Union(p1, p2) ->
      populate p1;
      populate p2
    | Link (s1,p1,s2,p2) ->
      Hashtbl.Poly.add_exn loc_table (s2,p2) (s1,p1);
      Hashtbl.Poly.add_multi switch_table s2 (s1,p1,p2);
    | p -> failwith (sprintf "Unexpected construct in policy: %s\n"
                       (Frenetic_NetKAT_Pretty.string_of_policy p)) in
  populate topo;
  (switch_table, loc_table)

let find_successors (topo:policy) =
  let open Frenetic_NetKAT in
  let switch_table = Hashtbl.Poly.create () in
  let loc_table = Hashtbl.Poly.create () in
  let rec populate pol = match pol with
    | Union(p1, p2) ->
      populate p1;
      populate p2
    | Link (s1,p1,s2,p2) ->
      Hashtbl.Poly.add_exn loc_table (s1,p1) (s2,p2);
      Hashtbl.Poly.add_multi switch_table s1 (s2,p2,p1);
    | p -> failwith (sprintf "Unexpected construct in policy: %s\n"
                       (Frenetic_NetKAT_Pretty.string_of_policy p)) in
  populate topo;
  (switch_table, loc_table)

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

(** Location-related functions *)
let combine_locations ?(hdr="Clash detected") p1 p2 = match p1, p2 with
  | (None    , None    ), (None,    None) ->
    (None, None)
  | (Some sw , None    ), (None,    Some pt)
  | (None    , Some pt ), (Some sw, None)
  | (Some sw , Some pt ), (None,    None)
  | (None    , None    ), (Some sw, Some pt) ->
    (Some sw, Some pt)
  | (Some sw , None    ), (None,    None)
  | (None    , None    ), (Some sw, None) ->
    (Some sw, None)
  | (None    , Some pt ), (None, None)
  | (None    , None    ), (None, Some pt) ->
    (None, Some pt)
  | (sw,pt), (sw',pt') ->
    let reason = begin match sw, pt, sw', pt' with
      | Some sw, _, Some sw', _ -> sprintf "Clashing switches %Ld and %Ld." sw sw'
      | _, Some pt, _, Some pt' -> sprintf "Clashing switches %ld and %ld." pt pt'
      | _ -> sprintf "No clash. Bug in code." end in
    let msg = String.concat ~sep:" " [hdr; reason] in
    raise (ClashException msg)

let locate_from_options (swopt, ptopt) : (loc, string) Result.t =
  match swopt, ptopt with
  | Some sw, Some pt -> Ok (sw,pt)
  | Some sw, None    -> Error (sprintf "No port specified for switch %Ld" sw)
  | None, Some pt    -> Error (sprintf "No switch specified for port %ld" pt)
  | None, None       -> Error "No switch or port specified"

let locate_from_header hv =
  let open Frenetic_NetKAT in
  match hv with
  | Switch sw -> (Some sw, None)
  | Location (Physical pt) -> (None, Some pt)
  | _ -> (None, None)

let rec locate_from_pred p =
  let open Frenetic_NetKAT in
  let hdr = "Clash in source" in
  match p with
    | Test hv -> locate_from_header hv
    | True | False | Neg _ -> (None, None)
    | And(p1, p2)
    | Or (p1, p2) ->
      combine_locations ~hdr:hdr (locate_from_pred p1) (locate_from_pred p2)

let locate_from_sink policy : (loc,string) Result.t =
  let open Frenetic_NetKAT in
  let hdr = "Clash in sinks" in
  let rec aux policy = match policy with
  | Mod hv         -> locate_from_header hv
  | Union (p1, p2) -> combine_locations ~hdr:hdr (aux p1) (aux p2)
  | Seq (p1, p2)   -> combine_locations ~hdr:hdr (aux p1) (aux p2)
  | Star p         -> aux p
  | _ -> (None, None) in
  locate_from_options (aux policy)

let locate_from_source policy =
  let open Frenetic_NetKAT in
  let hdr = "Clash in source" in
  let rec aux policy = match policy with
  | Filter f       -> locate_from_pred f
  | Union (p1, p2) -> combine_locations ~hdr:hdr (aux p1) (aux p2)
  | Seq (p1, p2)   -> combine_locations ~hdr:hdr (aux p1) (aux p2)
  | Star p         -> aux p
  | _              -> (None, None) in
  locate_from_options (aux policy)

(* TODO(basus): This should be implemented without converting to a policy *)
let locate_from_action (action: Action.t) =
  Action.to_policy action |> locate_from_sink

let locate_from_conditions (cs: condition list) =
  let open Frenetic_Fdd in
  let opts = List.fold_left cs ~init:(None, None)
      ~f:(fun (sw, pt) (field, pos, _) -> match field, pos with
          | Switch, Some v ->
            ( Some( Value.to_int64_exn v ), pt)
          | Location, Some v  ->
            (sw, Some( Value.to_int32_exn v ))
          | _ -> (sw, pt)) in
  locate_from_options opts

let locate_endpoints ((conds,action):stream) =
  let src  = locate_from_conditions conds in
  let sink = locate_from_action action in
  match src, sink with
  | Ok s, Ok s' -> Ok (s,s')
  | Ok _, Error s -> Error s
  | Error s, Ok _ -> Error s
  | Error s, Error s' -> Error (String.concat ~sep:"\n" [s;s'])

let locate (s:stream) =
  let locs = locate_endpoints s in
  match locs with
  | Ok (src,sink) -> Ok (src, sink, s)
  | Error e -> Error e

let locate_or_drop (located, dropped) (stream:stream) =
  let conds, action = stream in
  if (Action.to_policy action) = Frenetic_NetKAT.drop then (located, stream::dropped)
  else try match locate stream with
    | Ok l -> (l::located,dropped)
    | Error e ->
      (located, dropped)
    with ClashException e ->
      let msg = sprintf "Exception |%s| in alpha-beta pair: %s%!" e
          (string_of_stream stream) in
      print_endline msg;
      (located,dropped)

(** Start implementing graph-based retargeting. *)

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
    | Internal                  (* Between ports on the same switch *)
    | Exact of stream           (* The fabric delivers between those exact point *)
    | Adjacent of stream        (* Fabric delivers between locations attached to
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

let mk_mods path =
  let open OverEdge in
  let open Frenetic_NetKAT in
  match path with
  | (_,Adjacent (pred, pol),_)::_ ->
    Filter False
  | _ -> Filter True

let retarget (naive:stream list) (fabric:stream list) (topo:policy) =
  let open Frenetic_NetKAT in
  let naive_located, naive_dropped = List.fold naive ~init:([],[])
      ~f:locate_or_drop in
  let fabric_located, fabric_dropped = List.fold fabric ~init:([],[])
      ~f:locate_or_drop in
  let _, loc_preds = find_predecessors topo in
  let _, loc_succs = find_successors topo in

  (* Add vertices all naive locations *)
  let graph = List.fold naive_located ~init:Overlay.empty
      ~f:(fun g (src, sink, _) ->
          let g' = Overlay.add_vertex g src in
          Overlay.add_vertex g' sink) in

  (* Add edges between all location pairs reachable via the fabric *)
  let graph = ( List.fold fabric_located ~init:graph
      ~f:(fun g (src, sink, stream) ->
           let open OverEdge in
           let g' = Overlay.add_edge_e g (src, Exact stream, sink) in
           let src' = Hashtbl.Poly.find loc_preds src in
           let sink' = Hashtbl.Poly.find loc_succs sink in
           match src', sink' with
           | Some src', Some sink' ->
            Overlay.add_edge_e g' (src', Adjacent stream, sink')
           | _ -> g'
         ) ) in

  let graph = switch_inter_connect graph in

  (* Given a list of hops consisting of alternating fabric paths and internal
     forwards on edge switches, generate ingress, egress, or bounce rules to
     stitch the hops together, forming a VLAN-tagged path between the naive
     policy endpoints *)
  let rec stitch path tag stream =
    let open OverEdge in
    match path with
    | [] -> ([], [])
    | (_,Internal,(_,in_pt))::rest ->
      let mods = mk_mods rest in
      let filter = Filter (fst stream |> pred_of_conds) in
      let ingress = seq [ filter;
                          Mod( Vlan tag );
                          Mod( Location( Physical in_pt)) ] in
      let ins, outs = stitch rest tag stream in
      (ingress::ins, outs)
    | (_,Adjacent _,_)::((out_sw,out_pt),Internal,(_,in_pt))::rest ->
      let test_loc = And( Test( Switch out_sw ),
                          Test( Location( Physical out_pt ))) in
      let filter = Filter( And( Test( Vlan tag ),
                                test_loc )) in
      let egress = begin match rest with
        | [] ->
          let mods = snd stream |> Action.to_policy in
          seq [ filter; Mod( Vlan strip_vlan); remove_switch_mods mods ]
        | rest ->
          seq [ filter; Mod( Location( Physical in_pt )) ] end in
      let ins, outs = stitch rest tag stream in
      (ins, egress::outs)
    | _ -> failwith "Malformed path" in

  let ingresses, egresses, _ = List.fold naive_located ~init:([],[],1)
      ~f:(fun (ins, outs, tag) (src,sink,naive_stream) ->
          try
            let path,_ = OverPath.shortest_path graph src sink in
            let ingress, egress = stitch path tag naive_stream in
            ( List.rev_append ingress ins,
              List.rev_append egress  outs,
              tag + 1 )
          with Not_found ->
            printf "No path between %s and %s\n%!"
              (string_of_loc src) (string_of_loc sink);
            (ins,outs, tag)) in

  ingresses, egresses

let conds_of_pred (p:pred) : condition list list =
  let policy = Frenetic_NetKAT.Filter p in
  let streams = extract policy in
  List.fold_left streams ~init:[] ~f:(fun acc (conds, action) ->
      if (Action.to_policy action) = Frenetic_NetKAT.drop then acc
      else conds::acc)
