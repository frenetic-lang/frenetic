open Packet
open OpenFlow0x01_Core

type request =
  | DescriptionRequest
  | IndividualRequest of pattern * int8 * pseudoPort option
  | AggregateRequest of pattern * int8 * pseudoPort option


type descriptionStats =
    { manufacturer : string
    ; hardware : string
    ; software : string
    ; serial_number : string
    ; datapath : string
    }
      
 
type individualStats =
    { table_id : int8
    ; of_match : pattern
    ; duration_sec : int32 
    ; duration_nsec : int32 
    ; priority : int16
    ; idle_timeout : int16
    ; hard_timeout : int16
    ; cookie : int64
    ; packet_count : int64 
    ; byte_count : int64
    ; actions : action list 
    }

type aggregateStats =
    { total_packet_count : int64 
    ; total_byte_count : int64
    ; flow_count : int32 
    }

type reply =
  | DescriptionRep of descriptionStats
  | IndividualFlowRep of individualStats list
  | AggregateFlowRep of aggregateStats
