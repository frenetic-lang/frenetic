open Frenetic_NetKAT
open Frenetic_NetKAT_Optimize
open Frenetic_NetKAT_Pretty

module Tbl = Core.Std.Hashtbl.Poly
module Sexp = Core.Std.Sexp

(* auxilliary list functions *)
let inters xs ys = List.find_all (fun x -> List.mem x ys) xs
let product xs ys =
  List.fold_right (fun x l -> List.fold_right (fun y l -> (x,y)::l) ys l) xs []
let minimize xs obj =
  let f best x =
    let v = obj x in
    match best,v with
    | None, None -> None
    | None, Some v -> Some (x, v)
    | Some (y, v'),None -> best
    | Some (y, v'), Some v -> if v<v' then Some (x, v) else best
  in
  List.fold_left f None xs

(* physical location *)
type ploc = switchId * portId [@@deriving compare, eq]

(* virtual location *)
type vloc = vswitchId * vportId [@@deriving compare, eq]

(* topology node *)
type ('a, 'b) node =
  | InPort of 'a * 'b
  | OutPort of 'a * 'b
[@@deriving sexp, compare, eq]

(* virtual vertex *)
module VV = struct
  type t = (vswitchId, vportId) node [@@deriving sexp, compare, eq]
  let hash  = Hashtbl.hash
end

(* physical vertex *)
module PV = struct
  type t = (switchId, portId) node [@@deriving sexp, compare, eq]
  let hash = Hashtbl.hash
end

(* product vertex *)
type prod_vertex =
  | ConsistentIn of VV.t * PV.t
  | InconsistentOut of VV.t * PV.t
  | ConsistentOut of VV.t * PV.t
  | InconsistentIn of VV.t * PV.t
[@@deriving sexp, compare, eq]

module V = struct
  type t = prod_vertex [@@deriving sexp, compare, eq]
  let hash = Hashtbl.hash
end

(* Module to build graphs from topologies (physical or virtual) *)
module GraphBuilder (Params : sig
  type switch [@@deriving sexp]
  type port [@@deriving sexp]
  val locs_from_pred : pred -> (switch * port) list
  val links_from_topo : policy -> (switch * port * switch * port) list
end) (Vlabel : Graph.Sig.COMPARABLE with type t = (Params.switch, Params.port) node) = struct
  module G = struct
    include Params
    include Graph.Persistent.Digraph.Concrete(Vlabel)

    let sexp_of_vertex v = sexp_of_node sexp_of_switch sexp_of_port (V.label v)
    let add_vertex' v g = add_vertex g v
    let add_edge' v1 v2 g = add_edge g v1 v2

    let add_loc (sw, pt) g =
      let in_pt = V.create (InPort (sw, pt)) in
      let out_pt = V.create (OutPort (sw, pt)) in
      g |> add_vertex' in_pt
        |> add_vertex' out_pt

    let add_link (sw1, pt1, sw2, pt2) g =
      g |> add_loc (sw1, pt1)
        |> add_loc (sw2, pt2)
        |> add_edge' (V.create (OutPort (sw1, pt1))) (V.create (InPort (sw2, pt2)))

    let connect_switch_ports' v1 v2 g =
      match V.label v1, V.label v2 with
      | InPort (sw, _), OutPort (sw', _) when sw=sw' -> add_edge g v1 v2
      | _ -> g

    let connect_switch_ports g =
      fold_vertex (fun v1 g' -> fold_vertex (fun v2 g' -> connect_switch_ports' v1 v2 g') g g') g g

    let make (ingress : pred) (egress : pred) (topo : policy) =
      empty |> List.fold_right add_link (links_from_topo topo)
            |> List.fold_right add_loc (locs_from_pred ingress)
            |> List.fold_right add_loc (locs_from_pred egress)
            |> connect_switch_ports

    (* dot file encoding *)
    let graph_attributes g = []
    let default_vertex_attributes v = []
    let vertex_name v = "\"" ^ (Sexp.to_string (sexp_of_vertex v)) ^ "\""
    let vertex_attributes v = []

    let get_subgraph v =
      let open Graph.Graphviz.DotAttributes in
      match V.label v with
      | InPort (sw, _) | OutPort (sw, _) -> Some {
        sg_name = Sexp.to_string (sexp_of_switch sw);
        sg_attributes = [];
        sg_parent = None
      }

    let default_edge_attributes e = []
    let edge_attributes e = []
  end

  module Dot = Graph.Graphviz.Dot(G)
  include G

end



(* Module holding the three types of graphs we need: virtual, phyiscal, and product graphs *)
module G = struct

  module Virt = GraphBuilder (struct
    type switch = vswitchId [@@deriving sexp]
    type port = vportId [@@deriving sexp]
    let rec locs_from_pred pred =
      match pred with
      | And (Test (VSwitch vsw), Test (VPort vpt)) -> [(vsw, vpt)]
      | Or (p1, p2) -> locs_from_pred p1 @ locs_from_pred p2
      | _ -> failwith "Virtual Compiler: not a valid virtual ingress/egress predicate"
    let rec links_from_topo vtopo =
      match vtopo with
      | VLink (vsw1,vpt1,vsw2,vpt2) -> [(vsw1,vpt1,vsw2,vpt2)]
      | Union (t1, t2) -> links_from_topo t1 @ links_from_topo t2
      | _ -> if vtopo = drop then [] else
        failwith ("Virtual Compiler: not a valid virtual topology")
  end) (VV)

  module Phys = GraphBuilder (struct
    type switch = switchId [@@deriving sexp]
    type port = portId [@@deriving sexp]
    let rec locs_from_pred pred =
      match pred with
      | And (Test (Switch sw), Test (Location (Physical pt))) -> [(sw, pt)]
      | Or (p1, p2) -> locs_from_pred p1 @ locs_from_pred p2
      | _ -> failwith "Virtual Compiler: not a valid physical ingress/egress predicate"
    let rec links_from_topo topo =
      match topo with
      | Link (sw1,pt1,sw2,pt2) -> [(sw1, pt1, sw2, pt2)]
      | Union (t1, t2) -> links_from_topo t1 @ links_from_topo t2
      | _ -> if topo = drop then [] else failwith "Virtual Compiler: not a valid physical topology"
  end) (PV)

  module Prod = struct
    module G = struct
      include Graph.Persistent.Digraph.Concrete(V)
      let graph_attributes g = []
      let default_vertex_attributes v = []
      let vertex_name v = "\"" ^ (Sexp.to_string (sexp_of_prod_vertex (V.label v))) ^ "\""
      let vertex_attributes v = []
      let get_subgraph v =
        let open Graph.Graphviz.DotAttributes in
        match V.label v with
        | ConsistentIn (_, pv) | ConsistentOut (_, pv)
        | InconsistentIn (_, pv) | InconsistentOut (_, pv) ->
          begin match pv with
          | InPort (sw, _) | OutPort (sw, _) -> Some {
            sg_name = Sexp.to_string (sexp_of_switchId sw);
            sg_attributes = [];
            sg_parent = None
          }
          end
      let default_edge_attributes e = []
      let edge_attributes e = []
    end
    module Dot = Graph.Graphviz.Dot(G)
    include G
  end

end

let parse_vrel (vrel : pred) =
  let rec parse_physical pred alist =
    match pred with
    | Or (p1, p2) -> parse_physical p1 alist |> parse_physical p2
    | And (Test (Switch sw), Test (Location (Physical pt))) -> (sw, pt) :: alist
    | _ -> failwith "Virtual Compiler: not a valid virtual relation"
  in
  let rec parse pred alist =
    match pred with
    | Or (p1, p2) ->
       parse p1 alist |> parse p2
    | And (And (Test (VSwitch vsw), Test (VPort vpt)), physical) ->
       ((vsw, vpt), parse_physical physical []) :: alist
    | _ ->
       failwith "Virtual Compiler: not a valid virtual relation"
  in
  match Tbl.of_alist (parse vrel []) with
  | `Ok map -> map
  | `Duplicate_key (_, _) -> failwith "Virtual Compiler: virtual relation contains duplicate key"

let get_vrel vrel =
  let vrel_tbl = parse_vrel vrel in
  let vrel (vsw, vpt) = Tbl.find vrel_tbl (vsw, vpt) |> Core.Std.Option.value ~default:[] in
  (fun vv -> match G.Virt.V.label vv with
    | InPort (vsw, vpt) ->
       vrel (vsw, vpt) |> List.map (fun (sw, pt) -> G.Phys.V.create (InPort (sw, pt)))
    | OutPort (vsw, vpt) ->
       vrel (vsw, vpt) |> List.map (fun (sw, pt) -> G.Phys.V.create (OutPort (sw, pt))))

let make_product_graph (vgraph : G.Virt.t) (pgraph : G.Phys.t) (ving : pred) (vrel : pred) =
begin

  let vrel' = get_vrel vrel in

  let add_loop v g =
    match G.Phys.V.label v with
    | OutPort (sw,pt) -> G.Phys.add_edge g v (G.Phys.V.create (InPort (sw,pt)))
    | _ -> g
  in

  let pgraph_closure =
    let module Op = Graph.Oper.P(G.Phys) in
    let closure = Op.transitive_closure ~reflexive:false pgraph in
    G.Phys.fold_vertex add_loop closure closure
  in

  let virt_ing =
    List.map (fun (vsw, vpt) -> InPort (vsw, vpt) |> G.Virt.V.create) (G.Virt.locs_from_pred ving)
  in

  let prod_ing =
    List.map (fun vv -> product [vv] (vrel' vv)) virt_ing
    |> List.flatten
    |> List.map (fun (vv, pv) -> G.Prod.V.create (ConsistentIn (vv, pv)))
  in

  let step v =
    begin match G.Prod.V.label v with
    | ConsistentIn (vv, pv)  ->
       let virtual_sucs = G.Virt.succ vgraph vv in
       List.map (fun vv -> InconsistentOut (vv, pv) |> G.Prod.V.create) virtual_sucs
    | InconsistentOut (vv, pv) ->
       let physical_sucs =
         match vrel' vv with
         (* SJS: This is a hack. We interpret [] as true, although to be consistent we would have
                 to interpret it as false *)
         | [] -> G.Phys.succ pgraph_closure pv
         | logical_sucs -> inters logical_sucs (G.Phys.succ pgraph_closure pv) in
       List.map (fun psuc -> ConsistentOut (vv, psuc) |> G.Prod.V.create) physical_sucs
    | ConsistentOut (vv, pv) ->
       (* SJS: check that if there are no successors, we have reached the egress *)
       let virtual_sucs = G.Virt.succ vgraph vv in
       List.map (fun vsuc -> InconsistentIn (vsuc, pv) |> G.Prod.V.create) virtual_sucs
    | InconsistentIn (vv, pv) ->
       let physical_sucs =
         match vrel' vv with
         (* SJS: This is a hack. We interpret [] as true, although to be consistent we would have
                 to interpret it as false *)
         | [] -> G.Phys.succ pgraph_closure pv
         | logical_sucs -> inters logical_sucs (G.Phys.succ pgraph_closure pv) in
       List.map (fun pv -> ConsistentIn (vv, pv) |> G.Prod.V.create) physical_sucs
    end
  in

  let rec make work_list edges g =
    begin match work_list with
    | [] ->
       (* add edges after all vertices are inserted *)
       List.fold_left (fun g (v1, v2) -> G.Prod.add_edge g v1 v2) g edges
    | v::vs ->
       if G.Prod.mem_vertex g v then
         make vs edges g
       else
         let g' = G.Prod.add_vertex g v in
         let sucs = step v in
         let edges' = List.fold_left (fun edges suc -> (v, suc)::edges) edges sucs in
         make (sucs@work_list) edges' g'
    end
  in

  (prod_ing, make prod_ing [] (G.Prod.empty))

end



(* The fabric has to ensure that no matter what the programmer does, it can always restore
   consistency. This function eliminates all paths that allow the programmer to step to a
   unrepairable state. *)
let prune_product_graph g =
  (* an inconsistent location in the product graph is "fatal" if restoring consistency is
   impossible; here we exlude nodes that are fatal "by transitivity" *)
  let is_fatal v g =
    match G.Prod.V.label v with
    | InconsistentIn _ | InconsistentOut _ -> G.Prod.out_degree g v = 0
    | _ -> false
  in
  (* erases fatal node and all its predecessors that are fatal by transitivity *)
  let rec erase_fatal v g =
    match G.Prod.V.label v with
    | InconsistentIn _ | InconsistentOut _ ->
       let g' = G.Prod.remove_vertex g v in
       G.Prod.fold_pred erase_fatal g v g'
    | ConsistentOut _ | ConsistentIn _ ->
       let g' = G.Prod.remove_vertex g v in
       G.Prod.fold_pred (fun v g -> if is_fatal v g then erase_fatal v g else g) g v g'
  in
  G.Prod.fold_vertex (fun v g -> if is_fatal v g then erase_fatal v g else g) g g


(* The pruned graph may leave the fabric with several options to restore consistency; to arrive at
   a fabric graph, we must decide on a single option wherever we have a choice, thus determining a
   fabric uniquely.
   This function implements a greedy algorithm that makes this choice by minimizing the cost of the
   selection at each step, yielding a fabric valid for ingress ing. *)
let fabric_graph_of_pruned g ing cost =
  let rec select v g' =
    if G.Prod.mem_vertex g' v then g' else
    let g' = G.Prod.add_vertex g' v in
    match G.Prod.V.label v with
    | ConsistentIn _ | ConsistentOut _ ->
       G.Prod.fold_succ (select' v) g v g'
    | InconsistentIn _ | InconsistentOut _ ->
       let sucs = G.Prod.succ g v in
       begin match minimize sucs (fun v' -> cost v v') with
         | None -> assert false (*no alernate path*)
         | Some (selection, _) -> select' v selection g'
       end
  and select' v v' g' =
    G.Prod.add_edge (select v' g') v v'
  in
  List.fold_right select ing G.Prod.empty



(* functions for fabric generation *)
let match_ploc (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))
let match_vloc (vsw,vpt) = Filter (And (Test(VSwitch vsw), Test(VPort vpt)))
let set_vloc (vsw,vpt) = mk_seq (Mod (VSwitch vsw)) (Mod (VPort vpt))

let match_vloc' vv =
  match G.Virt.V.label vv with
  | InPort (vsw, vpt) | OutPort (vsw, vpt) -> match_vloc (vsw, vpt)

let match_ploc' pv =
  match G.Phys.V.label pv with
  | InPort (sw, pt) | OutPort (sw, pt) -> match_ploc (sw, pt)

let set_vloc' vv =
  match G.Virt.V.label vv with
  | InPort (vsw, vpt) | OutPort (vsw, vpt) -> set_vloc (vsw, vpt)

let rec policy_of_path path =
  match path with
  | (OutPort (sw1, pt1), (InPort (sw2, pt2))) :: path' ->
     if sw1 = sw2 then begin
       assert (pt1 = pt2);
       policy_of_path path'
     end
     else
       mk_seq (Link (sw1, pt1, sw2, pt2)) (policy_of_path path')
  | (InPort (sw, pt), (OutPort (sw', pt'))) :: path' ->
     assert (sw = sw');
     if pt = pt' then
       policy_of_path path'
     else
       mk_seq (Mod (Location (Physical (pt')))) (policy_of_path path')
  | [] -> id
  | _ -> assert false

let rec print_path path out_channel =
  match path with
  | (OutPort (sw1, _), (InPort (sw2, _))) :: path' ->
     if sw1 = sw2 then
       print_path path' out_channel
     else
       (Printf.fprintf out_channel "%Lu-%Lu" sw1 sw2;
       print_path path' out_channel)
  | (InPort (sw, _), (OutPort (sw', _))) :: path' ->
    Printf.fprintf out_channel " ";
    print_path path' out_channel
  | [] -> Printf.fprintf out_channel "\n%!";
  | _ -> assert false

let fabric_atom_of_prod_edge ?record_paths path_oracle v1 v2 =
  match G.Prod.V.label v1, G.Prod.V.label v2 with
  | ConsistentOut _, InconsistentIn _ | ConsistentIn _, InconsistentOut _ -> `None
  | (InconsistentOut (vv, pv1) as l), ConsistentOut (vv', pv2)
  | (InconsistentIn (vv, pv1) as l), ConsistentIn (vv', pv2) ->
    assert (vv = vv');
    let path = path_oracle pv1 pv2 in
    let _ = Core.Std.Option.(record_paths >>| print_path path) in
    let fabric =
      [match_vloc' vv; match_ploc' pv1; policy_of_path path; set_vloc' vv]
      |> mk_big_seq
    in
    begin match l with
     | InconsistentOut _ -> `Out fabric
     | InconsistentIn _ -> `In fabric
     | _ -> assert false
    end
  | _ -> assert false

let fabric_of_fabric_graph ?record_paths g ing path_oracle =
  if not (List.for_all (G.Prod.mem_vertex g) ing) then
    failwith "virtual compiler: specification allows for no valid fabric"
  else
    let record_paths = Core.Std.Option.(record_paths >>| open_out) in
    let f v1 v2 ((fout, fin) as fs) =
      match fabric_atom_of_prod_edge ?record_paths path_oracle v1 v2 with
      | `None -> fs
      | `Out f -> (f::fout, fin)
      | `In f -> (fout, f::fin) in
    let fabric = G.Prod.fold_edges f g ([], []) in
    let _ = Core.Std.Option.(record_paths >>| close_out) in
    fabric

let generate_fabrics ?(log=true) ?record_paths vrel v_topo v_ing v_eg p_topo p_ing p_eg  =
  let vgraph = G.Virt.make v_ing v_eg v_topo in
  let pgraph = G.Phys.make p_ing p_eg p_topo in
  let prod_ing, prod_graph = make_product_graph vgraph pgraph v_ing vrel in

  let unwrap_e e = (G.Phys.V.label (G.Phys.E.src e), G.Phys.V.label (G.Phys.E.dst e)) in
  let unwrap_path path = List.map unwrap_e path in

  let module WEIGHT = struct
    type edge = G.Phys.E.t
    type t = int
    let weight e =
      match unwrap_e e with
      | InPort _, OutPort _ -> 0
      | OutPort _, InPort _ -> 1
      | _, _ -> assert false
    let compare = compare
    let add x y = x + y
    let zero = 0
  end in

  let module Dijkstra = Graph.Path.Dijkstra(G.Phys)(WEIGHT) in
  let dist_tbl = Tbl.create () in

  let is_loop pv1 pv2 =
    match pv1, pv2 with
    | OutPort (sw, _), InPort (sw', _) -> sw = sw'
    | _ -> false
  in

  let get_path_and_distance pv1 pv2 =
    if is_loop pv1 pv2 then Some ([],0) else
    match Tbl.find dist_tbl (pv1, pv2) with
    | None ->
      begin try
        let path', dist = Dijkstra.shortest_path pgraph pv1 pv2 in
        let path = unwrap_path path' in
        Tbl.set dist_tbl ~key:(pv1, pv2) ~data:(path, dist);
        Some (path, dist)
      with Not_found ->
        None
      end
    | pd -> pd
  in

  let path_oracle pv1 pv2 =
    match get_path_and_distance pv1 pv2 with
    | Some (p,d) -> p
    | None -> assert false
  in

  let pv_of_v v =
    match G.Prod.V.label v with
    | InconsistentIn (_, pv) | InconsistentOut (_, pv)
    | ConsistentIn (_, pv) | ConsistentOut (_, pv) -> pv
  in

  let cost v1 v2 =
    match get_path_and_distance (pv_of_v v1) (pv_of_v v2) with
    | Some (p,d) -> Some p
    | None -> None
  in

  let pruned_graph = lazy (prune_product_graph prod_graph) in
  let fabric_graph = lazy (fabric_graph_of_pruned (Lazy.force pruned_graph) prod_ing cost) in
  let fabric = lazy (fabric_of_fabric_graph ?record_paths (Lazy.force fabric_graph) prod_ing path_oracle) in
  let vg_file = "vg.dot" in
  let pg_file = "pg.dot" in
  let g_raw_file = "g_raw.dot" in
  let g_pruned_file = "g_pruned.dot" in
  let g_fabric_file = "g_fabric.dot" in
  let vg_ch = open_out vg_file in
  let pg_ch = open_out pg_file in
  let g_raw_ch = open_out g_raw_file in
  let g_pruned_ch = open_out g_pruned_file in
  let g_fabric_ch = open_out g_fabric_file in
  begin
    if log then (
      Printf.printf "[virtual] Statistics:\n";
      Printf.printf "  |V(vgraph)|: %i\n" (G.Virt.nb_vertex vgraph);
      Printf.printf "  |E(vgraph)|: %i\n" (G.Virt.nb_edges vgraph);
      G.Virt.Dot.output_graph vg_ch vgraph;
      close_out vg_ch;
      Printf.printf "  |V(pgraph)|: %i\n" (G.Phys.nb_vertex pgraph);
      Printf.printf "  |E(pgraph)|: %i\n" (G.Phys.nb_edges pgraph);
      G.Phys.Dot.output_graph pg_ch pgraph;
      close_out pg_ch;
      Printf.printf "  |V(prod_graph)|: %i\n" (G.Prod.nb_vertex prod_graph);
      Printf.printf "  |E(prod_graph)|: %i\n" (G.Prod.nb_edges prod_graph);
      G.Prod.Dot.output_graph g_raw_ch prod_graph;
      close_out g_raw_ch;
      Printf.printf "  |V(pruned_graph)|: %i\n" (G.Prod.nb_vertex (Lazy.force pruned_graph));
      Printf.printf "  |E(pruned_graph)|: %i\n" (G.Prod.nb_edges (Lazy.force pruned_graph));
      G.Prod.Dot.output_graph g_pruned_ch (Lazy.force pruned_graph);
      close_out g_pruned_ch;
      Printf.printf "  |V(fabric_graph)|: %i\n" (G.Prod.nb_vertex (Lazy.force fabric_graph));
      Printf.printf "  |E(fabric_graph)|: %i\n" (G.Prod.nb_edges (Lazy.force fabric_graph));
      G.Prod.Dot.output_graph g_fabric_ch (Lazy.force fabric_graph);
      close_out g_fabric_ch;
      Printf.printf "\n");
    Lazy.force fabric
  end


(*
  Vingress defines the virtual ingress. Examples:

   1) The physical ingress is {(1,1)} (i.e. packets can enter the network only
      through port 1 of switch 1), and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   2) The physical ingress is {(1,1), (2,2)} (i.e. packets can enter the network only
      through port 1 of switch 1 and port 2 of switch 2),
      and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   3) The physical ingress is {(1,1)} and we want packets to enter the virtual network
      at both vport 3 of vswitch 3 and vport 4 of switch 4. This is encoded as
        vingress =
          (vswitch := 3; vport := 3) + (vswitch := 4; vport := 4)

   4) The physical ingress is {(1,1), (2,2)} and we want packets from (1,1) to
      enter the virtual network at vport 3 of vswitch 3, and packet from (2,2)
      shall enter at vport 4 of vswitch 4. This is encoded as
        vingress =
          switch = 1; port = 1; vswitch := 3; vport := 3
        + switch = 2; port = 2; vswitch := 4; vport := 4

   5) I just realized that the framework can even handle more complicated virtual ingress
      specifications as the ones Arjun mentioned in our last meeting, e.g.
        vingress =
          IPProto = tcp; vswitch := 1; vport := 1
        + IPProto = ucp; vswitch := 2; vport := 1
      This is super awesome!!


  To gurantee correctness we will have to do some sort of "type checking", i.e. we have to make sure
  certain pre conditions are met.

*)

let rec encode_vlinks (vtopo : policy) =
  match vtopo with
  | Union (p, q) -> mk_union (encode_vlinks p) (encode_vlinks q)
  | Seq (p, q) -> mk_seq (encode_vlinks p) (encode_vlinks q)
  | Star p -> mk_star (encode_vlinks p)
  | VLink (vsw1, vpt1, vsw2, vpt2) ->
    mk_seq
      (mk_filter (mk_and (Test (VSwitch vsw1)) (Test (VPort vpt1))))
      (mk_seq (Mod (VSwitch vsw2)) (Mod (VPort vpt2)))
  | _ -> vtopo

let compile ?(log=true) ?(record_paths : string option)
  ~(vrel : pred) ~(vtopo : policy) ~(ving_pol : policy) ~(ving : pred) ~(veg : pred)
                 ~(ptopo : policy)                      ~(ping : pred) ~(peg : pred)
  (vpol : policy) : policy =
  let (fout_set, fin_set) = generate_fabrics ~log ?record_paths vrel vtopo ving veg ptopo ping peg in
  let fout = mk_big_union fout_set in
  let fin = mk_big_union fin_set in
  let ing = mk_big_seq [Filter ping; ving_pol; Filter ving] in
  let eg = Filter (mk_and veg peg) in
  let p = mk_seq vpol fout in
  let t = mk_seq (encode_vlinks vtopo) fin in
  (* ing; (p;t)^*; p  *)
  (* Printf.printf "ing: %s\n\n%!" (NetKAT_Pretty.string_of_policy ing);
  Printf.printf "fout: %s\n\n%!" (NetKAT_Pretty.string_of_policy fout);
  Printf.printf "fin: %s\n\n%!" (NetKAT_Pretty.string_of_policy fin);
  Printf.printf "vpol: %s\n\n%!" (NetKAT_Pretty.string_of_policy vpol);
  Printf.printf "vtopo: %s\n\n%!" (NetKAT_Pretty.string_of_policy vtopo); *)
  mk_big_seq [ing; mk_star (mk_seq p t); p; eg]
