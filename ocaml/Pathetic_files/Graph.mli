module SwSet : Set.S
  with type elt = OpenFlowTypes.switchId

module type GRAPH =
sig
  type a = OpenFlowTypes.switchId
  type b = OpenFlowTypes.portId
  type h = int
  type graph
  val create : unit -> graph
  val add_switch : graph -> a -> unit
  val add_edge : graph -> a -> b -> a -> b -> unit
  val add_host_edge : graph -> h -> a -> b -> unit
  val shortest_path : graph -> a -> a -> a list
  val get_ports : graph -> a -> a -> (b*b)
  (* val get_ports : graph -> a -> b list *)
  val nodes : graph -> SwSet.t
  (* val get_other_port : graph -> a -> b -> (a*b) option *)
  val next_hop : graph -> a -> b -> a
  val get_host_port : graph -> h -> (a*b) option
  val get_nbrs : graph -> a -> a list
  val has_node : graph -> a -> bool
  val del_edge : graph -> a -> b -> unit
  val del_edges : graph -> (a*b) list -> unit
  val copy : graph -> graph
  val to_string : graph -> string
  exception NoPath of string*string
  exception NotFound of string
end
  with type a = Int64.t
  and type b = Int32.t

module Graph : GRAPH
