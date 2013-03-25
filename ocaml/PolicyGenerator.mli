module type DIRECTED_GRAPH = sig
  type node
  type edge_label
  type t
 
  val empty : unit -> t
  val add_edge : t -> node -> edge_label -> node -> unit
  val all_nodes : t -> node list

  val floyd_warshall : t -> (node * node list * node) list

  val path_with_edges : t -> node list -> (node * edge_label * node) list

  val to_string : t -> string
end 

module type PARAM = sig
  type node
  type edge_label
  val string_of_node : node -> string
  val string_of_edge_label : edge_label -> string
end


module Make : functor (Param : PARAM) -> 
  (DIRECTED_GRAPH 
   with type node = Param.node
   and type edge_label = Param.edge_label)

