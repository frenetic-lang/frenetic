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

  val marshal : portConfig -> int32


  val parse : int32 -> portConfig

  val to_string : portConfig -> string
end

module PortFeatures : sig

  val marshal : portFeatures -> int32

  val parse : int32 -> portFeatures

  val to_string : portFeatures -> string

end

module Oxm : sig

  type t = oxm

  val field_name : oxm -> string

  val sizeof : oxm -> int 

  val sizeof_header : oxm list -> int

  val to_string : oxm -> string

  val marshal : Cstruct.t -> oxm -> int

  val marshal_header : Cstruct.t -> oxm -> int

  val parse : Cstruct.t -> oxm * Cstruct.t

  val parse_header : Cstruct.t -> oxm * Cstruct.t

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

  val sizeof : oxmMatch -> int

  val to_string : oxmMatch -> string 


  val marshal : Cstruct.t -> oxmMatch -> int

  val parse : Cstruct.t -> oxmMatch * Cstruct.t

end

module Action : sig

  type sequence = OpenFlow0x04_Core.actionSequence

  val sizeof : action -> int

  val marshal : Cstruct.t -> action -> int

  val parse : Cstruct.t -> action

  val parse_sequence : Cstruct.t -> sequence

  val to_string :  action -> string
    
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

  val sizeof : flowModCommand -> int

  val marshal : t -> int

  val parse : int -> flowModCommand

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

  val to_string : instruction -> string

  val sizeof : instruction -> int

  val marshal : Cstruct.t -> instruction -> int

  val parse : Cstruct.t ->  instruction

end

module Instructions : sig

  val sizeof : instruction list -> int

  val marshal : Cstruct.t -> instruction list -> int

  val to_string : instruction list -> string

  val parse : Cstruct.t -> instruction list

end

module FlowMod : sig

  val sizeof : flowMod -> int

  val marshal : Cstruct.t -> flowMod -> int

  val parse : Cstruct.t -> flowMod

  val to_string : flowMod -> string

end

module Capabilities : sig
 
  val to_string : capabilities -> string

  val parse : int32  -> capabilities

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

  val marshal : portState -> int32

  val parse : int32 -> portState

  val to_string : portState -> string

end

module PortDesc : sig

  val sizeof : portDesc -> int

  val marshal : Cstruct.t -> portDesc -> int

  val parse : Cstruct.t -> portDesc

  val to_string : portDesc -> string
end

module PortStatus : sig

  val sizeof : portStatus -> int

  val marshal : Cstruct.t ->  portStatus -> int

  val parse : Cstruct.t -> portStatus

  val to_string : portStatus -> string

end

module PacketIn : sig

  val sizeof : packetIn -> int

  val to_string : packetIn -> string

  val marshal : Cstruct.t -> packetIn -> int

  val parse : Cstruct.t -> packetIn

end

module PacketOut : sig

  type t = packetOut

  val sizeof : packetOut -> int
  val to_string : packetOut -> string

  val marshal : Cstruct.t -> packetOut -> int
  val parse : Cstruct.t -> packetOut

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

  val sizeof : flowRequest -> int

  val to_string : flowRequest -> string

  val marshal : Cstruct.t -> flowRequest -> int

  val parse : Cstruct.t -> flowRequest

end

module QueueRequest : sig

  type t = queueRequest
    
  val marshal : Cstruct.t -> queueRequest -> int

  val parse : Cstruct.t -> queueRequest

  val sizeof : queueRequest -> int

  val to_string : queueRequest -> string

end

module TableFeatureProp : sig

  type t = tableFeatureProp

  val marshal : Cstruct.t -> tableFeatureProp -> int

  val parse : Cstruct.t -> tableFeatureProp

  val sizeof : tableFeatureProp -> int

  val to_string : tableFeatureProp -> string

end

module TableFeature : sig

  type t = tableFeatures

  val sizeof : tableFeatures -> int

  val marshal : Cstruct.t -> tableFeatures -> int

  val parse : Cstruct.t -> tableFeatures*Cstruct.t

  val to_string : tableFeatures -> string

end

module TableFeatures : sig

  type t = tableFeatures list

  val sizeof : tableFeatures list -> int

  val marshal : Cstruct.t -> tableFeatures list -> int

  val parse : Cstruct.t -> tableFeatures list

  val to_string : tableFeatures list -> string

end

module MultipartReq : sig

  type t = multipartRequest

  val sizeof : multipartRequest -> int

  val to_string : multipartRequest -> string

  val marshal : Cstruct.t -> multipartRequest -> int
 
  val parse : Cstruct.t -> multipartRequest

end

module GroupStats : sig

  module BucketStats : sig
  
    type t = bucketStats

    val sizeof : bucketStats -> int

    val to_string : bucketStats -> string
      
    val marshal : Cstruct.t -> bucketStats -> int

    val parse : Cstruct.t -> bucketStats

  end
  
  type t = groupStats

  val sizeof : groupStats -> int

  val to_string : groupStats -> string

  val marshal : Cstruct.t ->  groupStats -> int

  val parse : Cstruct.t ->  groupStats
end

module SwitchDescriptionReply : sig

  type t = switchDesc

  val sizeof : switchDesc -> int

  val to_string : switchDesc -> string

  val marshal : Cstruct.t -> switchDesc -> int

  val parse : Cstruct.t -> switchDesc

end


module FlowStats : sig

  type t = flowStats

  val sizeof : flowStats -> int

  val to_string : flowStats -> string

  val marshal : Cstruct.t -> flowStats -> int

  val parse : Cstruct.t -> flowStats

end


module AggregateStats : sig

  type t = aggregStats

  val sizeof : aggregStats -> int

  val to_string : aggregStats -> string

  val marshal : Cstruct.t -> aggregStats -> int

  val parse : Cstruct.t -> aggregStats

end

module TableStats : sig

  type t = tableStats

  val sizeof : tableStats -> int

  val to_string : tableStats -> string

  val marshal : Cstruct.t -> tableStats -> int

  val parse : Cstruct.t -> tableStats

end

module PortStats : sig
 
  type t = portStats

  val sizeof : portStats-> int

  val to_string : portStats -> string

  val marshal : Cstruct.t -> portStats -> int

  val parse : Cstruct.t -> portStats

end

module QueueStats : sig

  type t = queueStats

  val sizeof : queueStats -> int

  val to_string : queueStats -> string

  val marshal : Cstruct.t -> queueStats -> int

  val parse : Cstruct.t -> queueStats 

end

module GroupDesc : sig

  type t = groupDesc

  val sizeof : groupDesc -> int

  val to_string : groupDesc -> string

  val marshal : Cstruct.t -> groupDesc -> int

  val parse : Cstruct.t -> groupDesc

end

module GroupFeatures : sig

  type t = groupFeatures

  val sizeof : groupFeatures -> int

  val to_string : groupFeatures -> string

  val marshal : Cstruct.t -> groupFeatures -> int

  val parse : Cstruct.t -> groupFeatures 

end

module MeterStats : sig

  type t = meterStats

  val sizeof : meterStats -> int

  val to_string : meterStats -> string

  val marshal : Cstruct.t -> meterStats -> int

  val parse : Cstruct.t -> meterStats

end

module MeterConfig : sig

  type t = meterConfig

  val sizeof : meterConfig -> int

  val to_string : meterConfig -> string

  val marshal : Cstruct.t -> meterConfig -> int

  val parse : Cstruct.t -> meterConfig

end


module MeterFeaturesStats : sig

  type t = meterFeaturesStats

  val sizeof : meterFeaturesStats -> int

  val to_string : meterFeaturesStats -> string

  val marshal : Cstruct.t -> meterFeaturesStats -> int

  val parse : Cstruct.t -> meterFeaturesStats

end

module MultipartReply : sig

  type t = multipartReply

  val sizeof : multipartReply -> int

  val to_string : multipartReply -> string

  val marshal : Cstruct.t -> multipartReply -> int

  val parse : Cstruct.t -> multipartReply

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
    | FlowModMsg of flowMod
    | GroupModMsg of GroupMod.t
    | PortModMsg of portMod
    | MeterModMsg of meterMod
    | PacketInMsg of packetIn
    | FlowRemoved of flowRemoved
    | PacketOutMsg of packetOut
    | PortStatusMsg of portStatus
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
    | TableModMsg of tableMod
    | GetAsyncRequest
    | GetAsyncReply of asyncConfig
    | SetAsync of asyncConfig
    | Error of Error.t

  val sizeof : t -> int

  val to_string : t -> string

  val blit_message : t -> Cstruct.t -> int
  
  val header_of : xid -> t -> OpenFlow_Header.t

  val marshal : xid -> t -> string

  val parse : OpenFlow_Header.t -> string -> (xid * t)
  
  val marshal_body : t -> Cstruct.t -> unit
   
end
