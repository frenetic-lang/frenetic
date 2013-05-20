module Types : sig

  open Packet
  open Word

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

  val string_of_switchId : switchId -> string
      
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

  type reason =
    | NoMatch
    | ExplicitSend

  type packetIn = {
    packetInBufferId : bufferId option;
    packetInTotalLen : Word16.t; 
    packetInPort : portId;
    packetInReason : reason; 
    packetInPacket :  packet }

  type xid = Word32.t

  type packetOut = { 
    pktOutBufOrBytes : (bufferId, bytes) Misc.sum;
    pktOutPortId : portId option;
    pktOutActions : actionSequence 
  }

  (* Component types of stats_request messages. *)
  
  type table_id = Word8.t
  
  module IndividualFlowRequest : sig
      type t = { of_match : of_match
               ; table_id : table_id
               ; port : pseudoPort
               }
  end
  
  module AggregateFlowRequest : sig
      type t = { of_match : of_match
               ; table_id : table_id
               ; port : pseudoPort
               }
  end
  
  (* Component types of stats_reply messages. *)
  
  module DescriptionStats : sig
    type t = { manufacturer : string
             ; hardware : string
             ; software : string
             ; serial_number : string
             ; datapath : string
             }
  end
  
  module IndividualFlowStats : sig
      type t = { table_id : table_id
               ; of_match : of_match
               ; duration_sec : int
               ; duration_msec : int

               ; priority : int
               ; idle_timeout : int
               ; hard_timeout : int
               ; cookie : int
               ; byte_count : int
               ; actions : actionSequence
               }
  end
  
  module AggregateFlowStats : sig
      type t = { packet_count : int
               ; byte_count : int
               ; flow_count : int
               }
  end
  
  module TableStats : sig
      type t = { table_id : table_id
               ; name : string
               ; wildcards : Word32.t
               ; max_entries : int
               ; active_count : int
               ; lookup_count : int
               ; matched_count : int
               }
  end
  
  module PortStats : sig
      type t = { port_no : pseudoPort
               ; rx_packets : int
               ; tx_packets : int
               ; rx_bytes : int
               ; tx_bytes : int
               ; rx_dropped : int
               ; tx_dropped : int
               ; rx_errors : int
               ; tx_errors : int
               ; rx_frame_err : int
               ; rx_over_err : int
               ; rx_crc_err : int
               ; collisions : int
               }
  end
  
  type statsRequest =
  | DescriptionReq
  | IndividualFlowReq of IndividualFlowRequest.t
  | AggregateFlowReq of AggregateFlowRequest.t
  | TableReq
  | PortReq of pseudoPort
  (* TODO(cole): queue and vendor stats requests. *)
  
  type statsReply =
  | DescriptionRep of DescriptionStats.t
  | IndividualFlowRep of IndividualFlowStats.t
  | AggregateFlowRep of AggregateFlowStats.t
  | TableRep of TableStats.t
  | PortRep of PortStats.t
  
  (* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

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
    | StatsRequestMsg of statsRequest
    | StatsReplyMsg of statsReply

  (** A pattern that matches all packets. (All fields wildcarded.) *)
  val match_all : of_match

  (** A message ([FlowModMsg]) that deletes all flows. *)
  val delete_all_flows : message

  (** A permanent [FlowModMsg] adding a rule. *)
  val add_flow : of_match -> actionSequence -> message

end
  (* Ugliness only needed for the bonkers unverified Coq controller *)
  with type switchId = Int64.t

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

