module type GRAPH =
sig
  type node =
      Host of int
    | Switch of NetCore_Types.switchId

  type a = node
  type b = NetCore_Types.portId
  type graph
  val create : unit -> graph
  val add_node : graph -> a -> unit
  val add_switch : graph -> Int64.t -> unit
  val add_host : graph -> int -> unit
  val add_port : graph -> a -> b -> unit
  val add_edge : graph -> a -> b -> a -> b -> unit
  val add_host_edge : graph -> a -> a -> b -> unit
  val shortest_path : graph -> a -> a -> a list
  val get_ports : graph -> a -> a -> (b*b)
  val ports_of_switch : graph -> a -> b list
  val get_switches : graph -> NetCore_Types.switchId list
  val get_hosts : graph -> a list
  val get_nodes : graph -> a list
  val get_switches_and_ports : graph -> (NetCore_Types.switchId * b list) list
  (* val get_ports : graph -> a -> b list *)
  (* val get_other_port : graph -> a -> b -> (a*b) option *)
  val next_hop : graph -> a -> b -> a
  val get_nbrs : graph -> a -> a list
  val has_node : graph -> a -> bool
  val del_edge : graph -> a -> b -> unit
  val del_edges : graph -> (a*b) list -> unit
  val del_link : graph -> a -> a -> unit
  val del_links : graph -> (a*a) list -> unit
  val del_node : graph -> a -> unit
  val copy : graph -> graph
  val to_string : graph -> string
  val node_to_string : node -> string
  exception NoPath of string*string
  exception NotFound of string
end

module Graph : GRAPH
