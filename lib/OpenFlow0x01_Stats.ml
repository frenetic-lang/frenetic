open Packet
open OpenFlow0x01_Core

type individualStatsReq =
  { is_of_match : pattern
  ; is_table_id : int8
  ; is_out_port : pseudoPort option 
  }

type aggregateStatsReq =
  { as_of_match : pattern
  ; as_table_id : int8
  ; as_out_port : pseudoPort option
  }

type request =
  | DescriptionRequest
  | FlowTableStatsRequest
  | IndividualRequest of individualStatsReq
  | AggregateRequest of aggregateStatsReq

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

module Format = struct

  open Format

  let descriptionStats fmt v =
    fprintf fmt "@[{@[@[manufacturer=%s;@]@ @[hardware=%s;@]@ \
                      @[software=%s;@]@ @[serial=%s;@]@ @[datapath=%s@]@]}@]"
      v.manufacturer v.hardware v.software v.serial_number v.datapath

  (* TODO(arjun): must fill *)
  let individualStats fmt v = fprintf fmt "individualStats"

  let aggregateStats fmt v =
    fprintf fmt "@[{@[@[packets=%Ld;@]@ @[bytes=%Ld;@]@ @[flows=%ld@]@]}@]"
      v.total_packet_count v.total_byte_count v.flow_count

  let reply fmt v = match v with
    | DescriptionRep st -> descriptionStats fmt st
    | IndividualFlowRep st -> individualStats fmt st
    | AggregateFlowRep st -> aggregateStats fmt st

  let string_of_mk formatter x =
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    formatter fmt x;
    fprintf fmt "@?";
    Buffer.contents buf

end

let reply_to_string  = Format.string_of_mk Format.reply
