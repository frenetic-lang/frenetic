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

  val sizeof : bucket -> int

  val to_string : bucket -> string

  val marshal : Cstruct.t -> bucket -> int

  val parse : Cstruct.t -> bucket  
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

  val sizeof : groupMod -> int

  val marshal : Cstruct.t -> groupMod -> int

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

  val sizeof : packetOut -> int

  val marshal : Cstruct.t -> packetOut -> int

end

module FlowRequest : sig

    val sizeof : flowRequest -> int

    val to_string : flowRequest -> string

    val marshal : Cstruct.t -> flowRequest -> int

    val parse : Cstruct.t -> flowRequest

end

module QueueRequest : sig

    val marshal : Cstruct.t -> queueRequest -> int

    val parse : Cstruct.t -> queueRequest

    val sizeof : queueRequest -> int
    val to_string : queueRequest -> string
end

module TableFeatureProp : sig

    val marshal : Cstruct.t -> tableFeatureProp -> int
     
    val parse : Cstruct.t -> tableFeatureProp

    val sizeof : tableFeatureProp -> int
    val to_string : tableFeatureProp -> string

end

module TableFeature : sig

    val sizeof : tableFeatures -> int

    val marshal : Cstruct.t -> tableFeatures -> int

    val parse : Cstruct.t -> tableFeatures*Cstruct.t

    val to_string : tableFeatures -> string

end

module TableFeaturesRequest : sig

    val sizeof : tableFeaturesRequest -> int

    val marshal : Cstruct.t -> tableFeaturesRequest -> int

    val parse : Cstruct.t -> tableFeaturesRequest

    val to_string : tableFeaturesRequest -> string

end

module MultipartReq : sig

  val sizeof : multipartRequest -> int

  val to_string : multipartRequest -> string

  val marshal : Cstruct.t -> multipartRequest -> int
 
  val parse : Cstruct.t -> multipartRequest

end

module PortsDescriptionReply : sig

  val sizeof : portDesc list -> int
  val to_string : portDesc list -> string
  val marshal : Cstruct.t ->  portDesc list -> int

  val parse : Cstruct.t ->  portDesc list

end

module GroupStats : sig

  module BucketStats : sig

      val sizeof : bucketStats list -> int
      val to_string : bucketStats list -> string
      val marshal : Cstruct.t -> bucketStats list -> int
      val parse : Cstruct.t -> bucketStats list

  end

  val sizeof : groupStats list -> int
  val to_string : groupStats list -> string
  val marshal : Cstruct.t ->  groupStats list -> int

  val parse : Cstruct.t ->  groupStats list
end

module SwitchDescriptionReply : sig

  val sizeof : switchDesc -> int

  val to_string : switchDesc -> string

  val marshal : Cstruct.t -> switchDesc -> int

  val parse : Cstruct.t -> switchDesc

end


module FlowStats : sig

  val sizeof : flowStats list -> int
  val to_string : flowStats list -> string
  val marshal : Cstruct.t -> flowStats list -> int

  val parse : Cstruct.t -> flowStats list

end


module AggregateStats : sig

  val sizeof : aggregStats -> int
  val to_string : aggregStats -> string

  val marshal : Cstruct.t -> aggregStats -> int

  val parse : Cstruct.t -> aggregStats

end

module TableStats : sig

  val sizeof : tableStats list -> int

  val to_string : tableStats list -> string

  val marshal : Cstruct.t -> tableStats list -> int

  val parse : Cstruct.t -> tableStats list

end

module PortStats : sig
 
  val sizeof : portStats list-> int

  val to_string : portStats list -> string

  val marshal : Cstruct.t -> portStats list -> int

  val parse : Cstruct.t -> portStats list

end

module QueueStats : sig

  val sizeof : queueStats list -> int
  val to_string : queueStats list -> string
  val marshal : Cstruct.t -> queueStats list -> int
  val parse : Cstruct.t -> queueStats list 

end

module GroupDesc : sig

  val sizeof : groupDesc list -> int

  val to_string : groupDesc list -> string

  val marshal : Cstruct.t -> groupDesc list -> int

  val parse : Cstruct.t -> groupDesc list

end

module GroupFeatures : sig

  val sizeof : groupFeatures -> int

  val to_string : groupFeatures -> string

  val marshal : Cstruct.t -> groupFeatures -> int

  val parse : Cstruct.t -> groupFeatures 

end

module MeterStats : sig

  val sizeof : meterStats list -> int

  val to_string : meterStats list -> string

  val marshal : Cstruct.t -> meterStats list -> int

  val parse : Cstruct.t -> meterStats list

end

module MultipartReply : sig

  val sizeof : multipartReply -> int

  val to_string : multipartReply -> string

  val marshal : Cstruct.t -> multipartReply -> int

  val parse : Cstruct.t -> multipartReply

end

module TableMod : sig

    val sizeof : tableMod -> int

end

module Error : sig

  type t = {
    typ : int16;
    code : int16;
  }

  val parse : Cstruct.t -> t

  val to_string : t -> string

end

module Message : sig

  type t =
    | Hello
    | EchoRequest of bytes
    | EchoReply of bytes
    | FeaturesRequest
    | FeaturesReply of SwitchFeatures.t
    | FlowModMsg of flowMod
    | GroupModMsg of groupMod
    | PacketInMsg of packetIn
    | PacketOutMsg of packetOut
    | PortStatusMsg of portStatus
    | MultipartReq of multipartRequest
    | MultipartReply of multipartReply
    | BarrierRequest
    | BarrierReply
    | Error of Error.t

  val sizeof : t -> int

  val to_string : t -> string

  val blit_message : t -> Cstruct.t -> int
  
  val header_of : xid -> t -> OpenFlow_Header.t

  val marshal : xid -> t -> string

  val parse : OpenFlow_Header.t -> string -> (xid * t)
  
  val marshal_body : t -> Cstruct.t -> unit
   
end
