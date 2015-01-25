open Packet
open OpenFlow0x01_Core

type statsReq =
  { sr_of_match : pattern
  ; sr_table_id : int8
  ; sr_out_port : pseudoPort option
  }

type request =
  | DescriptionRequest
  | FlowTableStatsRequest
  | IndividualRequest of statsReq
  | AggregateRequest of statsReq
  | PortRequest of pseudoPort option

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

type portStats =
    { port_no : int16
    ; rx_packets : int64
    ; tx_packets : int64
    ; rx_bytes : int64
    ; tx_bytes : int64
    ; rx_dropped : int64
    ; tx_dropped : int64
    ; rx_errors : int64
    ; tx_errors : int64
    ; rx_frame_err : int64
    ; rx_over_err : int64
    ; rx_crc_err : int64
    ; collisions : int64
    }

type reply =
  | DescriptionRep of descriptionStats
  | IndividualFlowRep of individualStats list
  | AggregateFlowRep of aggregateStats
  | PortRep of portStats

module Format = struct

  open Format

  let descriptionStats fmt v =
    fprintf fmt "@[{@[@[manufacturer=%s;@]@ @[hardware=%s;@]@ \
                      @[software=%s;@]@ @[serial=%s;@]@ @[datapath=%s@]@]}@]"
      v.manufacturer v.hardware v.software v.serial_number v.datapath

  (* TODO(arjun): must fill *)
  let individualStats fmt v =
    fprintf fmt "individualStats"

  let aggregateStats fmt v =
    fprintf fmt "@[{@[@[packets=%Ld;@]@ @[bytes=%Ld;@]@ @[flows=%ld@]@]}@]"
      v.total_packet_count v.total_byte_count v.flow_count

  let portStats fmt v =
    fprintf fmt "@[{@[port_no=%d@ \
                      rx_packets=%Ld@ tx_packets=%Ld@ \
                      rx_bytes=%Ld@ tx_bytes=%Ld@ \
                      rx_dropped=%Ld@ tx_dropped=%Ld@ \
                      rx_errors=%Ld@ tx_errors=%Ld@ \
                      rx_frame_err=%Ld@ rx_over_err=%Ld@ rx_crc_err=%Ld@ \
                      collisions=%Ld@]}@]"
      v.port_no
      v.rx_packets v.tx_packets
      v.rx_bytes v.tx_bytes
      v.rx_dropped v.tx_dropped
      v.rx_errors v.tx_errors
      v.rx_frame_err v.rx_over_err v.rx_crc_err
      v.collisions

  let reply fmt v = match v with
    | DescriptionRep st -> descriptionStats fmt st
    | IndividualFlowRep st -> individualStats fmt st
    | AggregateFlowRep st -> aggregateStats fmt st
    | PortRep st -> portStats fmt st

  let string_of_mk formatter x =
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    formatter fmt x;
    fprintf fmt "@?";
    Buffer.contents buf

end

let reply_to_string  = Format.string_of_mk Format.reply
