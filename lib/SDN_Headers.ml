type header = 
  | Header of SDN_Types.field
  | Switch

type value = VInt.t

type payload = SDN_Types.payload


module Formatting = struct

  open Format

  (* TODO(nimish) : Can be changed in SDN_Types.format_feild, will save
     duplication of effort and can be carried into NetCore pretty parser *)
  let format_field (fmt : formatter) (f : SDN_Types.field) : unit =
      pp_print_string fmt
      (match f with
        | SDN_Types.InPort ->     "inPort"
        | SDN_Types.EthType ->    "dlTyp"
        | SDN_Types.EthSrc ->     "dlSrc"
        | SDN_Types.EthDst ->     "dlDst"
        | SDN_Types.Vlan ->       "vlan"
        | SDN_Types.VlanPcp ->    "vlanPcp"
        | SDN_Types.IPProto ->    "nwProto"
        | SDN_Types.IP4Src ->     "srcIP"
        | SDN_Types.IP4Dst ->     "dstIP"
        | SDN_Types.TCPSrcPort -> "tcpSrcPort"
        | SDN_Types.TCPDstPort -> "tcpDstPort")

    let value (fmt : formatter) (v : VInt.t) : unit =
      if (Int64.of_int (-1) = VInt.get_int64 v) then pp_print_string fmt "<none>"
      else VInt.format fmt v


    let header (fmt : formatter) (h : header) : unit = match h with
      | Header h' -> format_field fmt h'
      | Switch -> pp_print_string fmt "switch"

end

let format_header = Formatting.header

let format_value = Formatting.value

let header_to_string = NetCore_Util.make_string_of format_header

let value_to_string = NetCore_Util.make_string_of format_value

let compare_header = Pervasives.compare
