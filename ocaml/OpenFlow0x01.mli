module Types : sig

  open Packet.Types
  open WordInterface

  type of_match = {
    matchDlSrc : dlAddr option;
    matchDlDst : dlAddr option;
    matchDlTyp : dlTyp option;
    matchDlVlan : dlVlan option;
    matchDlVlanPcp : dlVlanPcp option;
    matchNwSrc : nwAddr option; 
    matchNwDst : nwAddr option;
    matchNwProto : nwProto option;
    matchNwTos : nwTos option;
    matchTpSrc : tpPort option;
    matchTpDst : tpPort option;
    matchInPort : portId option 
  }

  type capabilities = {
    flow_stats : bool;
    table_stats : bool;
    port_stats : bool; 
    stp : bool;
    ip_reasm : bool;
    queue_stats : bool; 
    arp_match_ip : bool 
  }

  type actions = { 
    output : bool;
    set_vlan_id : bool;
    set_vlan_pcp : bool;
    strip_vlan : bool; 
    set_dl_src : bool;
    set_dl_dst : bool;
    set_nw_src : bool;
    set_nw_dst : bool; 
    set_nw_tos : bool;
    set_tp_src : bool;
    set_tp_dst : bool;
    enqueue : bool;
    vendor : bool 
  }

  type features = { 
    switch_id : Word64.t;
    num_buffers : Word32.t;
    num_tables : Word8.t;
    supported_capabilities : capabilities;
    supported_actions : actions 
  }

  type flowModCommand =
    | AddFlow
    | ModFlow
    | ModStrictFlow
    | DeleteFlow
    | DeleteStrictFlow

  type switchId = Word64.t
      
  type priority = Word16.t
      
  type bufferId = Word32.t

  type pseudoPort =
    | PhysicalPort of portId
    | InPort
    | Flood
    | AllPorts
    | Controller of Word16.t

  type action =
    | Output of pseudoPort
    | SetDlVlan of dlVlan
    | SetDlVlanPcp of dlVlanPcp
    | StripVlan
    | SetDlSrc of dlAddr
    | SetDlDst of dlAddr
    | SetNwSrc of nwAddr
    | SetNwDst of nwAddr
    | SetNwTos of nwTos
    | SetTpSrc of tpPort
    | SetTpDst of tpPort

  type actionSequence = action list

  type timeout =
    | Permanent
    | ExpiresAfter of Word16.t

  type flowMod = {
    mfModCmd : flowModCommand; 
    mfMatch : of_match;
    mfPriority : priority; 
    mfActions : actionSequence;
    mfCookie : Word64.t; mfIdleTimeOut : timeout;
    mfHardTimeOut : timeout; mfNotifyWhenRemoved : bool;
    mfApplyToPacket : bufferId option;
    mfOutPort : pseudoPort option; mfCheckOverlap : bool }

  type packetInReason =
    | NoMatch
    | ExplicitSend

  type packetIn = {
    packetInBufferId : bufferId option;
    packetInTotalLen : Word16.t; 
    packetInPort : portId;
    packetInReason_ : packetInReason; 
    packetInPacket :  packet }

  type xid = Word32.t

  type packetOut = { 
    pktOutBufOrBytes : (bufferId, bytes) Datatypes.sum;
    pktOutPortId : portId option;
    pktOutActions : actionSequence 
  }

  type message =
    | Hello of bytes
    | EchoRequest of bytes
    | EchoReply of bytes
    | FeaturesRequest
    | FeaturesReply of features
    | FlowModMsg of flowMod
    | PacketInMsg of packetIn
    | PacketOutMsg of packetOut
    | BarrierRequest
    | BarrierReply

end
  (* Ugliness only needed for the bonkers unverified Coq controller *)
  with type message = OpenFlow0x01Types.message
  and type features = OpenFlow0x01Types.features
  and type switchId = Int64.t


module Parser : sig

  module Header : sig

    type t

  end

  module Message : sig

    open Types

    type t = message
        
    val parse : Header.t -> Cstruct.t -> (xid * t) option
    val to_string : t -> string
    val marshal : xid -> t -> string

  end

end

(** Interface for all platforms. *)
module type PLATFORM = sig

  open Types


  (** [SwitchDisconnected switch_id] is raised by [send_to_switch] and
      [recv_from_switch]. This exception is only raised once per switch.
      If the functions are applied to [switch_id] again, they raise 
      [Invalid_argument]. *)
  exception SwitchDisconnected of switchId 
      
  (** [send_to_switch switch_id xid msg] sends [msg] to the switch,
      blocking until the send completes. *)
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t

  (** [recv_from_switch switch_id] blocks until [switch_id] sends a
      message. 
      
      If the switch sends an [ECHO_REQUEST], [recv_from_switch] will
      itself respond with an [ECHO_REPLY] and block for the next
      message. *)
  val recv_from_switch : switchId -> (xid * message) Lwt.t
    
  (** [accept_switch] blocks until a switch connects, handles the
      OpenFlow handshake, and returns after the switch sends a
      [FEATURES_REPLY] message. *)
  val accept_switch : unit -> features Lwt.t 
end

module Platform : sig

  (** An implementation of [PLATFORM]. The documentation for
      [PLATFORM] describes its features. *)

  open Lwt_unix
  open Types
    
  include PLATFORM
    
  (** [init_with_port p] accepts connections from switches on port [p], which
      is usually [6633]. *)
  val init_with_port : int -> unit

  (** [init_with_port fd] accepts connections from switches on [fd]. *)
  val init_with_fd : file_descr -> unit

  (** [shutdown] gracefully shuts down the server *) 
  val shutdown : unit -> unit 

end

module TestPlatform : sig

  open Types

  include PLATFORM

  val send_to_controller : switchId -> xid -> message -> unit Lwt.t

  val recv_from_controller : switchId -> (xid * message) Lwt.t

  val connect_switch : switchId -> unit Lwt.t

  val tear_down : unit -> unit

end

