open Packet
open OpenFlow0x04_Core

type msg_code =  | HELLO | ERROR | ECHO_REQ | ECHO_RESP | VENDOR | FEATURES_REQ
                 | FEATURES_RESP | GET_CONFIG_REQ | GET_CONFIG_RESP 
                 | SET_CONFIG | PACKET_IN | FLOW_REMOVED | PORT_STATUS | PACKET_OUT
                 | FLOW_MOD | GROUP_MOD | PORT_MOD | TABLE_MOD | MULTIPART_REQ
                 | MULTIPART_RESP | BARRIER_REQ | BARRIER_RESP | QUEUE_GET_CONFIG_REQ
                 | QUEUE_GET_CONFIG_RESP | ROLE_REQ | ROLE_RESP | GET_ASYNC_REQ
                 | GET_ASYNC_REP | SET_ASYNC | METER_MOD 

val msg_code_to_int : msg_code -> int

(** See the [ofp_port_config] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortConfig : sig

  type t = portConfig

  val marshal : t -> int32

  val parse : int32 -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_port_features] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortFeatures : sig

  type t = portFeatures

  val marshal : t -> int32

  val parse : int32 -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Flow Match Fields structure. See the section 7.2.3.2 of the OpenFlow 1.3.4 specification *)
module Oxm : sig

  type t = oxm

  val field_name : t -> string

  (** [sizeof t] size of the oxm field *)
  val sizeof : t -> int

  (** [sizeof_header t] size of the oxm field without the payload *)
  val sizeof_header : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  (** [marshal buf t] serializes [t] *)
  val marshal : Cstruct.t -> t -> int

  (** [marshal_header buf t] same as [marshal] but doesn't serialize the payload *)
  val marshal_header : Cstruct.t -> t -> int

  (** [parse bits] parse the buffer [bits] *)
  val parse : Cstruct.t -> t * Cstruct.t

  (** [parse_header bits] same as [parse] but doesn't parse the payload *)
  val parse_header : Cstruct.t -> t * Cstruct.t

end

module PseudoPort : sig

  type t = pseudoPort

  val size_of : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : t -> int32

  val make : int32 -> int16 -> t

end

(** Queue Description structure. See the section 7.2.2 of the OpenFlow 1.3.4 specification *)
module QueueDesc : sig

  (** Queue Property Description structure. See the 7.2.2 of the OpenFlow 1.3.4 specification *)
  module QueueProp : sig

    type t = queueProp

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = queueDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Switch Configuration structure. See the section 7.3.2 of the OpenFlow 1.3.4 specification *)
module SwitchConfig : sig

  type t = switchConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end  

(** Flow Match structure. See the section 7.2.3.1 of the OpenFlow 1.3.4 specification *)
module OfpMatch : sig

  type t = oxmMatch

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t * Cstruct.t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Action structure. See the section 7.2.5 of the OpenFlow 1.3.4 specification *)
module Action : sig

  type t = action

  type sequence = OpenFlow0x04_Core.actionSequence

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val parse_sequence : Cstruct.t -> sequence

  (** [to_string v] pretty-prints [v] *)
  val to_string :  t -> string
    
end

(** Bucket structure for use in groups. See the section 7.3.4.2 of OpenFlow 1.3.4 specification *)
module Bucket : sig

  type t = bucket

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t  

end

(** Modify Flow message structure. See the section 7.3.4.1 of the OpenFlow 1.3.4 specification *)
module FlowModCommand : sig
    
  type t = flowModCommand

  val sizeof : t -> int

  val marshal : t -> int

  val parse : int -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_group_type] enumeration in section 7.3.4.2 of the OpenFlow 1.3.4 specification *)
module GroupType : sig
    
  type t = groupType

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : t -> int

  val parse : int -> t

end

(** Modify Group message structure. See the section 7.3.4.2 of the OpenFlow 1.3.4 specification *)
module GroupMod : sig

  type t = groupMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Port message structure. See the section 7.3.4.3 of the OpenFlow 1.3.4 specification *)
module PortMod : sig

  type t = portMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Meter message structure. See the section 7.3.4.4 of the OpenFlow 1.3.4 specification *)
module MeterMod : sig

  type t = meterMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Instruction structure. See the section 7.2.4 of the OpenFlow 1.3.4 specification *)
module Instruction : sig

  type t = instruction

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t ->  t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

module Instructions : sig

  type t = instruction list

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Modify flow message structure. See the section 7.3.4.1 of the OpenFlow 1.3.4 specification *)
module FlowMod : sig

  type t = flowMod

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_capabilities] enumeration in section 7.3.1 of OpenFlow 1.3.4 specification *)
module Capabilities : sig

  type t = capabilities
 
 (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val to_int32 : t -> int32

  val parse : int32  -> t

end

(** Switch Features structure. See the section 7.3.1 of the OpenFlow 1.3.4 specification *)
module SwitchFeatures : sig

  type t = { datapath_id : int64; num_buffers : int32;
             num_tables : int8; aux_id : int8;
             supported_capabilities : capabilities }

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** See the [ofp_port_state] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortState : sig

  type t = portState

  val marshal : portState -> int32

  val parse : int32 -> portState

  (** [to_string v] pretty-prints [v] *)
  val to_string : portState -> string

end

(** Description of a port structure. See the section 7.3.1 of the OpenFlow 1.3.4 specification *)
module PortDesc : sig

  type t = portDesc

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Port Status structure. See the section 7.4.3 of the OpenFlow 1.3.4 specification *)
module PortStatus : sig

  type t = portStatus

  val sizeof : t -> int

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Packet received by the datapath and sent to the controller structure. See the section
    7.4.1 of the OpenFlow 1.3.4 specification *)
module PacketIn : sig

  type t = packetIn

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Packet send out of the datapath structure. See the section 7.3.7 of the OpenFlow 1.3.4 specification *)
module PacketOut : sig

  type t = packetOut

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter bands structure. See the section 7.3.4.4 of the OpenFlow 1.3.4 specification *)
module MeterBand : sig

  type t = meterBand

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Removed structure. See the section 7.4.2 of the OpenFlow 1.3.4 specification *)
module FlowRemoved : sig

  type t = flowRemoved

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Statistics request structure. See the section 7.3.5.2 of the OpenFlow 1.3.4 specification 
    this structure is the same for indidual and aggregate flow request *)
module FlowRequest : sig

  type t = flowRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Statistics request structure. See the section 7.3.5.8 of the OpenFlow 1.3.4 specification *)
module QueueRequest : sig

  type t = queueRequest
    
  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val sizeof : t -> int

  val to_string : t -> string

end

(** Table Features property structure. See the section 7.3.5.5.2 of the OpenFlow 1.3.4 specification *)
module TableFeatureProp : sig

  type t = tableFeatureProp

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Table Feature structure. See the section 7.3.5.5.1 of the OpenFlow 1.3.4 specification *)
module TableFeature : sig

  type t = tableFeatures

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Multipart request message structure. See the section 7.3.5 of the OpenFlow 1.3.4 specification *)
module MultipartReq : sig

  type t = multipartRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int
 
  val parse : Cstruct.t -> t

end

(** Group statistics structure. See the section 7.3.5.9 of the OpenFlow 1.3.4 specification *)
module GroupStats : sig

  (** Bucket statistics structure. See the section 7.3.5.9 of the OpenFlow 1.3.4 specification *)
  module BucketStats : sig
  
    type t = bucketStats

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string
      
    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end
  
  type t = groupStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t ->  t
end

(** Switch Description structure. See the section 7.3.5.1 of the OpenFlow 1.3.4 specification *)
module SwitchDescriptionReply : sig

  type t = switchDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Individual Flow Statistics structure. See the section 7.3.5.2 of the OpenFlow 1.3.4 specification *)
module FlowStats : sig

  type t = flowStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Aggregate Flow Statistics structure. See the section 7.3.5.3 of the OpenFlow 1.3.4 specification *)
module AggregateStats : sig

  type t = aggregStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Table Statistics structure. See the section 7.3.5.4 of the OpenFlow 1.3.4 specification *)
module TableStats : sig

  type t = tableStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Port Statistics structure. See the section 7.3.5.6 of the OpenFlow 1.3.4 specification *)
module PortStats : sig
 
  type t = portStats

  val sizeof : t-> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Statistics structure. See the section 7.3.5.8 of the OpenFlow 1.3.4 specification *)
module QueueStats : sig

  type t = queueStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Group Description structure. See the section 7.3.5.10 of the OpenFlow 1.3.4 specification *)
module GroupDesc : sig

  type t = groupDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Group Features structure. See the section 7.3.5.10 of the OpenFlow 1.3.4 specification *)
module GroupFeatures : sig

  type t = groupFeatures

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Statistics structure. See the section 7.3.5.12 of the OpenFlow 1.3.4 specification *)
module MeterStats : sig

  type t = meterStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Configuration structure. See the section 7.3.5.13 of the OpenFlow 1.3.4 specification *)
module MeterConfig : sig

  type t = meterConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Features structure. See the section 7.3.5.14 of the OpenFlow 1.3.4 specification *)
module MeterFeatures : sig

  type t = meterFeatures

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Multipart reply message structure. See the section 7.3.5 of the OpenFlow 1.3.4 specification *)
module MultipartReply : sig

  type t = multipartReply

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Table message structure. See the section 7.3.3 of the OpenFlow 1.3.4 specification *)
module TableMod : sig

  type t = tableMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Configuration request message structure. See the section 7.3.6 of the OpenFlow 1.3.4 specification *)
module QueueConfReq : sig

  type t = queueConfReq

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Configuration respond message structure. See the section 7.3.6 of the OpenFlow 1.3.4 specification *)
module QueueConfReply : sig

  type t = queueConfReply

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Error message structure. See the section 7.4.4 of the OpenFlow 1.3.4 specification *)
module Error : sig

  type t = {
    err : errorTyp;
    data : bytes;
  }

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Role Request message structure. See the section 7.3.9 of OpenFlow 1.3.4 specification *)
module RoleRequest : sig

  type t = roleRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Hello message structure. See the section 7.5.1 of the OpenFlow 1.3.4 specification *)
module Hello : sig

  (** Hello Element structure. See the section 7.5.1 of OpenFlow 1.3.4 specification *)
  module Element : sig

    (** Supported Version Bitmap structure. See the section 7.5.1. of OpenFlow 1.3.4 specification *)
    module VersionBitMap : sig

      type t = supportedList

      val sizeof : t -> int

      (** [to_string v] pretty-prints [v] *)
      val to_string : t -> string

      val marshal : Cstruct.t -> t -> int

      val parse : Cstruct.t -> t
    
    end
  
    type t = element

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = helloElement

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Set Asynchronous message structure. See the section 7.3.10 of OpenFlow 1.3.4 specification *)
module AsyncConfig : sig

  type t = asyncConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string 

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module Message : sig

  type t =
    | Hello of element list
    | EchoRequest of bytes
    | EchoReply of bytes
    | FeaturesRequest
    | FeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | GroupModMsg of GroupMod.t
    | PortModMsg of PortMod.t
    | MeterModMsg of MeterMod.t
    | PacketInMsg of PacketIn.t
    | FlowRemoved of FlowRemoved.t
    | PacketOutMsg of PacketOut.t
    | PortStatusMsg of PortStatus.t
    | MultipartReq of MultipartReq.t
    | MultipartReply of MultipartReply.t
    | BarrierRequest
    | BarrierReply
    | RoleRequest of RoleRequest.t
    | RoleReply of RoleRequest.t
    | QueueGetConfigReq of QueueConfReq.t
    | QueueGetConfigReply of QueueConfReply.t
    | GetConfigRequestMsg
    | GetConfigReplyMsg of SwitchConfig.t
    | SetConfigMsg of SwitchConfig.t
    | TableModMsg of TableMod.t
    | GetAsyncRequest
    | GetAsyncReply of AsyncConfig.t
    | SetAsync of AsyncConfig.t
    | Error of Error.t

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val blit_message : t -> Cstruct.t -> int
  
  val header_of : xid -> t -> OpenFlow_Header.t

  val marshal : xid -> t -> string

  val parse : OpenFlow_Header.t -> string -> (xid * t)
  
  val marshal_body : t -> Cstruct.t -> unit
   
end
