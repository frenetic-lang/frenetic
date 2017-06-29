open Core
open Runner
open ProbNetKAT
open ProbNetKAT.Syntax.Dumb

let build_list (n : int) ~(f : int -> 'a) : 'a list =
  let rec loop (i : int): 'a list =
    if i = n then []
    else (f i) :: loop (i + 1) in
  loop 0

type core = { k: int; core: int }
type aggregation =  { k: int; cluster: int }
type edge = { k: int; cluster: int; edge: int }
type host = { k: int; cluster: int; edge: int; host: int }

type switch
  = Core of core
  | Aggregation of aggregation
  | Edge of edge
  | Host of host

module Switch : sig
  val get_id : switch -> int
end = struct

  let next_id = ref 0

  let known_switches = Hashtbl.Poly.create ()

  let get_id sw =
    let default () =
      let id = !next_id in
      next_id := id + 1;
      id in
    Hashtbl.Poly.find_or_add known_switches sw ~default

end

(******* routing policy ************************)
(* Each switch has the same policy: spray packets on downlink ports *)

let pkt_spray_pol ~(k : int) : policy =
  (* There are k ports (numbered 1 to k),
     randomly forward traffic through each port *)
  choicei k ~f:(fun outport -> (!!("Port", outport + 1), 1//k))

let core_switch_policy ~(k : int) (core : int) : policy =
  let pol = pkt_spray_pol k in
  let sw_id = Switch.get_id (Core { k; core }) in
  ??("Switch", sw_id) >> pol

let core_policy ~(k : int) : policy =
  core_switch_policy ~k 0

let aggregation_switch_policy ~(k : int) (cluster : int) : policy =
  let pol = pkt_spray_pol k in
  let sw_id = Switch.get_id (Aggregation { k; cluster }) in
  ??("Switch", sw_id) >> pol

let aggregation_policy ~(k : int) : policy =
  build_list k ~f:(aggregation_switch_policy ~k) |> mk_big_union

let edge_switch_policy ~(k : int) ~(cluster : int) ~(edge : int) : policy =
  let pol = pkt_spray_pol k in
  let sw_id = Switch.get_id (Edge { k; cluster; edge }) in
  ??("Switch", sw_id) >> pol

let edge_policy ~(k : int) : policy =
  build_list k ~f:(fun cluster ->
    build_list k ~f:(fun edge ->
      edge_switch_policy ~k ~cluster ~edge)
    |> mk_big_union)
  |> mk_big_union

let routing_policy ~(k : int) : policy =
  let core = core_policy ~k in
  let aggregation = aggregation_policy ~k in
  let edge = edge_policy ~k in
  mk_big_union [core; aggregation; edge]


(************* Topology program for a simple tree *****************)
(* Port 0 of each switch connects to it's parent.
   Ports 1 to k connect a switch to it's k children from left to right *)

let core_agg_links ~(k : int) : policy =
  (* A single core switch (root) is connected to k aggregation switches *)
  let core = 0 in
  let core_sw_id = Switch.get_id (Core { k; core }) in
  build_list k ~f:(fun cluster ->
      let agg_sw_id = Switch.get_id (Aggregation { k; cluster }) in
      mk_union
        (??("Switch", core_sw_id) >> ??("Port", cluster + 1) >>
         !!("Switch", agg_sw_id) >> !!("Port", 0))
        (??("Switch", agg_sw_id) >> ??("Port", 0) >>
         !!("Switch", core_sw_id) >> !!("Port", cluster + 1)))
  |> mk_big_union

let agg_edge_links ~(k : int) : policy =
  (* Each aggregation switch is connected to k edge switches *)
  build_list k ~f:(fun cluster ->
    let agg_sw_id = Switch.get_id (Aggregation { k; cluster }) in
    build_list k ~f:(fun edge ->
      let edge_sw_id = Switch.get_id (Edge { k; cluster; edge }) in
        mk_union
          (??("Switch", agg_sw_id) >> ??("Port", edge + 1) >>
           !!("Switch", edge_sw_id) >> !!("Port", 0))
          (??("Switch", edge_sw_id) >> ??("Port", 0) >>
           !!("Switch", agg_sw_id) >> !!("Port", edge + 1)))
    |> mk_big_union)
  |> mk_big_union

let access_links ~(k : int) : policy =
  (* Each edge switch is connected to k hosts *)
  build_list k ~f:(fun cluster ->
    build_list k ~f:(fun edge ->
      let edge_sw_id = Switch.get_id (Edge { k; cluster; edge }) in
      build_list k ~f:(fun host ->
        let host_id = Switch.get_id (Host { k; cluster; edge; host}) in
        mk_union
          (??("Switch", edge_sw_id) >> ??("Port", host + 1) >>
           !!("Switch", host_id) >> !!("Port", 0))
          (??("Switch", host_id) >> ??("Port", 0) >>
           !!("Switch", edge_sw_id) >> !!("Port", host + 1)))
      |> mk_big_union)
    |> mk_big_union)
  |> mk_big_union

let topology_program ~(k : int) : policy =
  mk_big_union [(core_agg_links ~k); (agg_edge_links ~k); (access_links ~k)]

(* Policy to test if a packet has reached a server *)
let delivered_to_host ~(k : int) : policy =
  build_list k ~f:(fun cluster ->
    build_list k ~f:(fun edge ->
      build_list k ~f:(fun host ->
        let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
        ??("Switch", host_id))
      |> mk_big_union)
    |> mk_big_union)
  |> mk_big_union

(* Policy to generate packets at core *)
let in_traffic ~(k : int) : policy =
  let core = 0 in
  let core_sw_id = Switch.get_id (Core { k; core }) in
  !!("Switch", core_sw_id) >> !!("Port", 0)

(* Load-balancer program with traffic *)
let lb_policy ~(k : int) : policy =
  let ingress = in_traffic ~k in
  let t = topology_program ~k in
  let p = routing_policy ~k in
  let egress = delivered_to_host ~k in
  (* TODO: with while *)
  (* ingress >> mk_while (neg egress) (p >> t) >> egress *)
  ingress >> (p >> t) >> (p >> t) >> (p >> t) >> egress

let () = begin
  let k = 2 in
  run (lb_policy ~k)
end
