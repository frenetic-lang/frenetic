type node = 
    | Switch of SDN_Types.switchId
    | Host of Packet.dlAddr * Packet.nwAddr

module Node : sig
  type t = node

  val compare : t -> t -> int
  val to_string : t -> string

  (** Not yet implemented *)
  val parse_dot : Graph.Dot_ast.node_id -> Graph.Dot_ast.attr list -> t

  (** Not yet implemented *)
  val parse_gml : Graph.Gml.value_list -> t

end

module Edge : sig
  type t = unit

  val compare : t -> t -> int
  val to_string : t -> string

  val default : t

  (** Not yet implemented *)
  val parse_dot : Graph.Dot_ast.attr list -> t

  (** Not yet implemented *)
  val parse_gml : Graph.Gml.value_list -> t

end

module Net : Network.NETWORK 
  with module Topology.Vertex = Node
   and module Topology.Edge = Edge

module Protocol : sig
  open Core.Std
  open Async.Std

  type t

  module SwitchMap : Map.S
    with type Key.t = SDN_Types.switchId

  module PortSet : Set.S
    with type Elt.t = Int32.t

  val create
    : ?nib:Net.Topology.t
    -> unit
    -> t

  val state
    : t -> Net.Topology.t

  val setup_probe
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> OpenFlow0x01.SwitchFeatures.t
    -> t Deferred.t

  val setup_arp
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> t Deferred.t

  val remove_switch
    : t
    -> SDN_Types.switchId
    -> t

  val handle_port_status
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PortStatus.t
    -> t Deferred.t

  val handle_probe
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

  val handle_arp
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

end

