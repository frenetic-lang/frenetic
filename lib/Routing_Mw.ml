open Frenetic_Network
open Routing_Types
module Topology = Net.Topology

module type MW_INPUT = sig

  (* This is the type of some structure defined on the graph (e.g. a path
     or tree) that gives information on how to route through the graph. *)
  type structure

  (* Gets the next structure (e.g. path) that we are going to augment the
   * flow by, along with the weight of that structure. *)
  val select_structure : topology -> demands -> Net.Topology.VertexSet.t -> (structure * float)

  (* Gets the usages of every edge in the topology. *)
  val usage_of_structure : topology -> demands  -> structure -> (edge * float) list

  (* Gets the usage of one edge in the topology. *)
  val get_weight : topology -> edge -> float

  (* Sets the weight of one edge in the topology. *)
  val set_weight : topology -> edge -> float -> topology

end

module type MW_ALG = sig

  type structure

  val hedge : float -> float -> topology -> demands -> Topology.VertexSet.t
    -> (edge, float) Hashtbl.t
    -> (structure * float * topology * (edge, float) Hashtbl.t)

  val hedge_iterations : float -> topology -> demands -> Topology.VertexSet.t ->
    (int * (structure * float) list * topology)

end

module VertexSet = Topology.VertexSet

module Make = functor (Experts : MW_INPUT) -> struct

  open Experts
  type structure = Experts.structure

  let exp_weight epsilon usage = exp (epsilon *. usage)

  (* TODO(cy): rewrite most of this to eliminate clutter from
   * previous versions *)

  let hedge (epsilon : float) (delta : float) (topo : topology) (d:demands)
      (nodes : Topology.VertexSet.t) (usage_table : (edge, float) Hashtbl.t) =
    let (struc,c_min) = select_structure topo d nodes in
    let usage_vec = usage_of_structure topo d struc in

    let max_usage = List.fold_left (fun acc (_, usage) ->
        max acc usage) 0. usage_vec in

    (* Scale usages s.t. max is 1 (width reduction) *)
    let scaled_usage = List.map (fun (edge, usage) ->
        (edge, usage /. max_usage)) usage_vec in

    (* Add cumulative usage *)
    List.iter (fun (edge, usage) ->
        let old_usage = Hashtbl.find usage_table edge in
        let new_usage = old_usage +. usage in
        Hashtbl.replace usage_table edge new_usage) scaled_usage;

    let sum_weights = Hashtbl.fold (fun edge usage acc ->
        acc +. (exp_weight epsilon usage)) usage_table 0. in

    (* Reweight edges in graph *)
    let (reweighted : topology) = Hashtbl.fold (fun edge usage t ->
        let weight = (exp_weight epsilon usage) /. sum_weights in
        set_weight t edge weight) usage_table topo in

    (struc, 1. /. max_usage, reweighted, usage_table)

  (* This uses the functions defined in the above input module to run
     the multiplicative weights algorithm. The outputs are, in this
     order:

     1. The number of iterations the algorithm ran for.
     2. A list of structures (e.g. paths or trees) with associated
        weights.  The intended use is that when something needs to be
        routed, one should normalize and use the weights as a
        probability distribution, select a structure accordingly, and
        route using that structure.
     3. The original topology, but with weights modified to reflect
        the final state of the algorithm. *)
  let hedge_iterations (epsilon : float) (topo : topology) (d:demands)
      (nodes : VertexSet.t) =
    let num_edges = Topology.num_edges topo in
    if num_edges = 0 then
      let struc,_ = select_structure topo d nodes in
      (1, [(struc, 1.)], topo)
    else
      let delta = (epsilon ** 2.0) /.
                  (log (float_of_int num_edges)) in
      let rec loop n trees topo acc_usage table =
        let (new_tree, w, new_topo, new_tb) =
          hedge epsilon delta topo d nodes table in
        if w +. acc_usage >= 1. then
          let final_weight = 1. -. acc_usage in
          (n+1, (new_tree, final_weight)::trees, new_topo)
        else
          loop (n+1) ((new_tree,w)::trees) new_topo (w +. acc_usage) new_tb in
      let init_table = Hashtbl.create 8 in
      Topology.iter_edges (fun e -> Hashtbl.add init_table e 0.) topo;
      loop 0 [] topo 0. init_table

end
