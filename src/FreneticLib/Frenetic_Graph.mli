module type GRAPH =
sig
  type node =
    | Switch of OpenFlow0x01.switchId
    | Host of Packet.dlAddr

  type n = node
  type p = Packet.portId
  type graph
  val create : unit -> graph
  val add_node : graph -> n -> unit
  val add_switch : graph -> Int64.t -> unit
  val add_host : graph -> int -> unit
  val add_edge : graph -> n -> p -> n -> p -> unit
  val add_host_edge : graph -> n -> n -> p -> unit
  val shortest_path : graph -> n -> n -> n list
  val get_ports : graph -> n -> n -> (p*p)
  val get_switches : graph -> n list
  val get_hosts : graph -> n list
  val get_nodes : graph -> n list
  (* val get_ports : graph -> n -> b list *)
  (* val get_other_port : graph -> n -> b -> (a*b) option *)
  val next_hop : graph -> n -> p -> n
  val get_nbrs : graph -> n -> n list
  val has_node : graph -> n -> bool
  val del_edge : graph -> n -> p -> unit
  val del_edges : graph -> (n*p) list -> unit
  val del_link : graph -> n -> n -> unit
  val del_links : graph -> (n*n) list -> unit
  val del_node : graph -> n -> unit
  val copy : graph -> graph
  exception NoPath of node*node
  exception NotFound of node
end

module Graph : GRAPH
