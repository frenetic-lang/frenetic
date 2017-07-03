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
type match_action = policy * policy

module type Routing = sig
  val routing_policy : int -> policy
end

module PerHop : Routing = struct
  (* Each switch has the same policy: spray packets on downlink ports *)

  let pkt_spray_pol ~(k : int) : policy =
    (* There are k ports (numbered 1 to k),
       randomly forward traffic through each port *)
    choicei k ~f:(fun outport -> (!!("Port", outport + 1), 1//k))

  let core_switch_policy ~(k : int) (core : int) : match_action =
    let pol = pkt_spray_pol k in
    let sw_id = Switch.get_id (Core { k; core }) in
    (??("Switch", sw_id), pol)

  let core_policy ~(k : int) : match_action list =
    [core_switch_policy ~k 0]

  let aggregation_switch_policy ~(k : int) (cluster : int) : match_action =
    let pol = pkt_spray_pol k in
    let sw_id = Switch.get_id (Aggregation { k; cluster }) in
    (??("Switch", sw_id), pol)

  let aggregation_policy ~(k : int) : match_action list =
    build_list k ~f:(aggregation_switch_policy ~k)

  let edge_switch_policy ~(k : int) ~(cluster : int) ~(edge : int) : match_action =
    let pol = pkt_spray_pol k in
    let sw_id = Switch.get_id (Edge { k; cluster; edge }) in
    (??("Switch", sw_id), pol)

  let edge_policy ~(k : int) : match_action list =
    build_list k ~f:(fun cluster ->
      build_list k ~f:(fun edge ->
        edge_switch_policy ~k ~cluster ~edge))
    |> List.concat_no_order

  let routing_policy k : policy =
    let core = core_policy ~k in
    let aggregation = aggregation_policy ~k in
    let edge = edge_policy ~k in
    mk_big_ite ~default:drop (List.concat_no_order [core; aggregation; edge])
end

module CoreDecision : Routing = struct
  (* Core switch decides the destination server and updates the destination IP.
     Forwarding is done based on destination IP *)

  let core_switch_policy ~(k : int) (core : int) : match_action =
    (* Pick a destination IP at uniform. *)
    let lb_pol =
      build_list k ~f:(fun cluster ->
        build_list k ~f:(fun edge ->
          build_list k ~f:(fun host ->
            let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
            (!!("DestIP", host_id), 1//(k*k*k))))
        |> List.concat_no_order)
      |> List.concat_no_order
      |> choice in
  (* Destination based forwarding *)
    let route_pol =
      build_list k ~f:(fun cluster ->
        build_list k ~f:(fun edge ->
          build_list k ~f:(fun host ->
            let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
            (??("DestIP", host_id), !!("Port", cluster+1))))
        |> List.concat_no_order)
      |> List.concat_no_order
      |> mk_big_ite ~default:drop in
  let sw_id = Switch.get_id (Core { k; core }) in
  (??("Switch", sw_id), lb_pol >> route_pol)

  let core_policy ~(k : int) : match_action list =
    [core_switch_policy ~k 0]

  let aggregation_switch_policy ~(k : int) (cluster : int) : match_action =
    (* Destination based forwarding *)
    let pol =
      build_list k ~f:(fun edge ->
        build_list k ~f:(fun host ->
          let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
          (??("DestIP", host_id), !!("Port", edge+1))))
      |> List.concat_no_order
      |> mk_big_ite ~default:drop in
    let sw_id = Switch.get_id (Aggregation { k; cluster }) in
    (??("Switch", sw_id), pol)

  let aggregation_policy ~(k : int) : match_action list =
    build_list k ~f:(aggregation_switch_policy ~k)

  let edge_switch_policy ~(k : int) ~(cluster : int) ~(edge : int) : match_action =
    (* Destination based forwarding *)
    let pol =
        build_list k ~f:(fun host ->
          let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
          (??("DestIP", host_id), !!("Port", edge+1)))
      |> mk_big_ite ~default:drop in

    let sw_id = Switch.get_id (Edge { k; cluster; edge }) in
    (??("Switch", sw_id), pol)

  let edge_policy ~(k : int) : match_action list =
    build_list k ~f:(fun cluster ->
      build_list k ~f:(fun edge ->
        edge_switch_policy ~k ~cluster ~edge))
    |> List.concat_no_order

  let routing_policy k : policy =
    let core = core_policy ~k in
    let aggregation = aggregation_policy ~k in
    let edge = edge_policy ~k in
    mk_big_ite ~default:skip (List.concat_no_order [core; aggregation; edge])
end

module EcmpLite : Routing = struct
  (* Each switch decides next hop based on hash of a particular field. The
     field depends on the layer of switch. Core - SrcPort, Aggregation -
     DstPort, Edge - SrcIP. This is deterministic, and we assume we know the
     hash values. *)

  let core_switch_policy ~(k : int) (core : int) : match_action =
    let pol =
      build_list k ~f:(fun cluster ->
            (??("SrcPortHash%k", cluster), !!("Port", cluster+1)))
      |> mk_big_ite ~default:drop in
  let sw_id = Switch.get_id (Core { k; core }) in
  (??("Switch", sw_id), pol)

  let core_policy ~(k : int) : match_action list =
    [core_switch_policy ~k 0]

  let aggregation_switch_policy ~(k : int) (cluster : int) : match_action =
    let pol =
      build_list k ~f:(fun edge ->
            (??("DstPortHash%k", edge), !!("Port", edge+1)))
      |> mk_big_ite ~default:drop in
    let sw_id = Switch.get_id (Aggregation { k; cluster }) in
    (??("Switch", sw_id), pol)

  let aggregation_policy ~(k : int) : match_action list =
    build_list k ~f:(aggregation_switch_policy ~k)

  let edge_switch_policy ~(k : int) ~(cluster : int) ~(edge : int) : match_action =
    let pol =
      build_list k ~f:(fun host ->
            (??("SrcIPHash%k", host), !!("Port", host+1)))
      |> mk_big_ite ~default:drop in
    let sw_id = Switch.get_id (Edge { k; cluster; edge }) in
    (??("Switch", sw_id), pol)

  let edge_policy ~(k : int) : match_action list =
    build_list k ~f:(fun cluster ->
      build_list k ~f:(fun edge ->
        edge_switch_policy ~k ~cluster ~edge))
    |> List.concat_no_order

  let routing_policy k : policy =
    let core = core_policy ~k in
    let aggregation = aggregation_policy ~k in
    let edge = edge_policy ~k in
    mk_big_ite ~default:skip (List.concat_no_order [core; aggregation; edge])
end

(************* Topology program for a simple tree *****************)
(* Port 0 of each switch connects to it's parent.
   Ports 1 to k connect a switch to it's k children from left to right *)
type link = policy * policy

let core_agg_links ~(k : int) : link list =
  (* A single core switch (root) is connected to k aggregation switches *)
  let core = 0 in
  let core_sw_id = Switch.get_id (Core { k; core }) in
  build_list k ~f:(fun cluster ->
      let agg_sw_id = Switch.get_id (Aggregation { k; cluster }) in
      [(??("Switch", core_sw_id) >> ??("Port", cluster + 1),
        !!("Switch", agg_sw_id) >> !!("Port", 0));
       (??("Switch", agg_sw_id) >> ??("Port", 0),
        !!("Switch", core_sw_id) >> !!("Port", cluster + 1))])
  |> List.concat_no_order

let agg_edge_links ~(k : int) : link list =
  (* Each aggregation switch is connected to k edge switches *)
  build_list k ~f:(fun cluster ->
    let agg_sw_id = Switch.get_id (Aggregation { k; cluster }) in
    build_list k ~f:(fun edge ->
      let edge_sw_id = Switch.get_id (Edge { k; cluster; edge }) in
      [(??("Switch", agg_sw_id) >> ??("Port", edge + 1),
        !!("Switch", edge_sw_id) >> !!("Port", 0));
       (??("Switch", edge_sw_id) >> ??("Port", 0),
        !!("Switch", agg_sw_id) >> !!("Port", edge + 1))])
    |> List.concat_no_order)
  |> List.concat_no_order

let access_links ~(k : int) : link list =
  (* Each edge switch is connected to k hosts *)
  build_list k ~f:(fun cluster ->
    build_list k ~f:(fun edge ->
      let edge_sw_id = Switch.get_id (Edge { k; cluster; edge }) in
      build_list k ~f:(fun host ->
        let host_id = Switch.get_id (Host { k; cluster; edge; host}) in
        [(??("Switch", edge_sw_id) >> ??("Port", host + 1),
          !!("Switch", host_id) >> !!("Port", 0));
         (??("Switch", host_id) >> ??("Port", 0),
          !!("Switch", edge_sw_id) >> !!("Port", host + 1))])
      |> List.concat_no_order)
    |> List.concat_no_order)
  |> List.concat_no_order

let topology_program ~(k : int) : policy =
  let core = core_agg_links ~k in
  let aggregation = agg_edge_links ~k in
  let edge = access_links ~k in
  mk_big_ite ~default:drop (List.concat_no_order [core; aggregation; edge])

(* Policy to test if a packet has reached a server *)
let delivered_to_host ~(k : int) : policy =
  (build_list k ~f:(fun cluster ->
    build_list k ~f:(fun edge ->
      build_list k ~f:(fun host ->
        let host_id = Switch.get_id (Host { k; cluster; edge; host }) in
        ??("Switch", host_id)))
    |> List.concat_no_order)
  |> List.concat_no_order
  |> mk_big_union ~init:drop)
  >> ??("Port", 0)

(* Policy to test if a packet is still inside the network *)
let in_network ~(k : int) : policy =
  let core = 0 in
  let core_sw_id = Switch.get_id (Core { k; core }) in
  build_list k ~f:(fun cluster ->
    let aggregation_sw_id = Switch.get_id (Aggregation { k; cluster }) in
    build_list k ~f:(fun edge ->
      let edge_sw_id = Switch.get_id (Edge { k; cluster; edge }) in
      ??("Switch", edge_sw_id))
    |> mk_big_union ~init:??("Switch", aggregation_sw_id))
  |> mk_big_union ~init:??("Switch", core_sw_id)

(* Policy to generate packets at core *)
let in_traffic ~(k : int) : policy =
  let core = 0 in
  let core_sw_id = Switch.get_id (Core { k; core }) in
  ??("Switch", core_sw_id) >> ??("Port", 0)

(* Load-balancer program with traffic *)
let lb_policy ~(k : int) =
  let ingress = in_traffic ~k in
  let t = topology_program ~k in
  let p = PerHop.routing_policy k in
  (* let p = CoreDecision.routing_policy k in *)
  (* let p = EcmpLite.routing_policy k in *)
  let egress = delivered_to_host ~k in
  let within_network = in_network ~k in
  let pol = ingress >> mk_while (neg egress) (p >> t) >> egress in
  (* let pol = ingress >> mk_while (within_network) (p >> t) >> egress in *)
  (* let pol = ingress >> (p >> t) >> (p >> t) >> (p >> t) >> egress in *)
  run pol ~row_query:ingress ~col_query:egress
let () = begin
  let k = 2 in
  lb_policy ~k
end
