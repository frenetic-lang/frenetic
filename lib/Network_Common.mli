open Network
module Node : sig
  include VERTEX
  type device = Switch | Host | Middlebox

  val default : t
  val create : string -> int64 -> device -> int32 -> int64 -> t
  val name : t -> string
  val id : t -> int64
  val device : t -> device
  val mac : t -> int64
  val ip : t -> int32

end

module Link : sig
  include EDGE

  val create : int64 -> int64 -> t
  val cost : t -> int64
  val capacity : t -> int64
end

module Net : NETWORK 
  with type Topology.Vertex.t = Node.t
  and type Topology.Edge.t = Link.t

module Path : Net.PATH 
  with type weight = Int64.t
