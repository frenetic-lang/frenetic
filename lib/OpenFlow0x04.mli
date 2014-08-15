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

module PortConfig : sig

  type t = portConfig

  val marshal : t -> int32

  val parse : int32 -> t

  val to_string : t -> string
end

module PortFeatures : sig

  type t = portFeatures

  val marshal : t -> int32

  val parse : int32 -> t

  val to_string : t -> string

end

module Oxm : sig

  type t = oxm

  val field_name : t -> string

  val sizeof : t -> int

  val sizeof_header : t list -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val marshal_header : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t * Cstruct.t

  val parse_header : Cstruct.t -> t * Cstruct.t

end

module PseudoPort : sig

  type t = pseudoPort

  val size_of : t -> int

  val to_string : t -> string

  val marshal : t -> int32

  val make : int32 -> int16 -> t

end

module QueueDesc : sig

  module QueueProp : sig

    type t = queueProp

    val sizeof : t -> int

    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = queueDesc

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module SwitchConfig : sig

  type t = switchConfig

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end  

module OfpMatch : sig

  type t = oxmMatch

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t * Cstruct.t

  val to_string : t -> string
end

module Action : sig

  type t = action
  type sequence = OpenFlow0x04_Core.actionSequence

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val parse_sequence : Cstruct.t -> sequence

  val to_string :  t -> string
    
end

module Bucket : sig

  type t = bucket

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t  
end

module FlowModCommand : sig
    
  type t = flowModCommand

  val sizeof : t -> int

  val marshal : t -> int

  val parse : int -> t

  val to_string : t -> string

end

module GroupType : sig
    
  type t = groupType

  val to_string : t -> string

  val marshal : t -> int

  val parse : int -> t

end

module GroupMod : sig

  type t = groupMod

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module PortMod : sig

  type t = portMod

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module MeterMod : sig

  type t = meterMod

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module Instruction : sig

  type t = instruction

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t ->  t

  val to_string : t -> string

end

module Instructions : sig

  type t = instruction list

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module FlowMod : sig

  type t = flowMod

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module Capabilities : sig

  type t = capabilities
 
  val to_string : t -> string

  val parse : int32  -> t

end

module SwitchFeatures : sig

  type t = { datapath_id : int64; num_buffers : int32;
             num_tables : int8; aux_id : int8;
             supported_capabilities : capabilities }

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end


module PortState : sig

  type t = portState

  val marshal : portState -> int32

  val parse : int32 -> portState

  val to_string : portState -> string

end

module PortDesc : sig

  type t = portDesc

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module PortStatus : sig

  type t = portStatus

  val sizeof : t -> int

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module PacketIn : sig

  type t = packetIn

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module PacketOut : sig

  type t = packetOut

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module MeterBand : sig

  type t = meterBand

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module FlowRemoved : sig

  type t = flowRemoved

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module FlowRequest : sig

  type t = flowRequest

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module QueueRequest : sig

  type t = queueRequest
    
  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  val to_string : t -> string

end

module TableFeatureProp : sig

  type t = tableFeatureProp

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  val to_string : t -> string

end

module TableFeature : sig

  type t = tableFeatures

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t * Cstruct.t

  val to_string : t -> string

end

module TableFeatures : sig

  type t = tableFeatures list

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module MultipartReq : sig

  type t = multipartRequest

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int
 
  val parse : Cstruct.t -> t

end

module GroupStats : sig

  module BucketStats : sig
  
    type t = bucketStats

    val sizeof : t -> int

    val to_string : t -> string
      
    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end
  
  type t = groupStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t ->  t
end

module SwitchDescriptionReply : sig

  type t = switchDesc

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end


module FlowStats : sig

  type t = flowStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end


module AggregateStats : sig

  type t = aggregStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module TableStats : sig

  type t = tableStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module PortStats : sig
 
  type t = portStats

  val sizeof : t-> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module QueueStats : sig

  type t = queueStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module GroupDesc : sig

  type t = groupDesc

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module GroupFeatures : sig

  type t = groupFeatures

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module MeterStats : sig

  type t = meterStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module MeterConfig : sig

  type t = meterConfig

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end


module MeterFeaturesStats : sig

  type t = meterFeaturesStats

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module MultipartReply : sig

  type t = multipartReply

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module TableMod : sig

  type t = tableMod

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module QueueConfReq : sig

  type t = queueConfReq

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module QueueConfReply : sig

  type t = queueConfReply

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module Error : sig

  type t = {
    err : errorTyp;
    data : bytes;
  }

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  val to_string : t -> string

end

module RoleRequest : sig

  type t = roleRequest

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module Hello : sig

  module Element : sig

    module VersionBitMap : sig

      type t = supportedList

      val sizeof : t -> int

      val to_string : t -> string

      val marshal : Cstruct.t -> t -> int

      val parse : Cstruct.t -> t
    
    end
  
    type t = element

    val sizeof : t -> int

    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = helloElement

  val sizeof : t -> int

  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module AsyncConfig : sig

  type t = asyncConfig

  val sizeof : t -> int

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

  val to_string : t -> string

  val blit_message : t -> Cstruct.t -> int
  
  val header_of : xid -> t -> OpenFlow_Header.t

  val marshal : xid -> t -> string

  val parse : OpenFlow_Header.t -> string -> (xid * t)
  
  val marshal_body : t -> Cstruct.t -> unit
   
end
