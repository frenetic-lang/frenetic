open List
open Packet
open Format

type 'a wildcard = 'a NetCore_Wildcard.wildcard

type port =
  | Physical of portId
  | All
  | Here

module PortOrderedType = struct
  type t = port
  let compare = Pervasives.compare
  let to_string =  function
    | Physical pid -> "Physical " ^ (portId_to_string pid)
    | All -> "All"
    | Here -> "Here"

end

module DlVlanOrderedType = struct
  type t = int option

  let compare x y = match (x, y) with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some a, Some b -> Pervasives.compare a b

  let to_string x = match x with
    | Some n -> "Some " ^ string_of_int n
    | None -> "None"
end

module Int64Wildcard = NetCore_Wildcard.Make (Int64)
module Int32Wildcard = NetCore_Wildcard.Make (Int32)
module IntWildcard =  NetCore_Wildcard.Make (struct
  type t = int
  let compare = Pervasives.compare
  let to_string n = string_of_int n
end)

module DlAddrWildcard = Int64Wildcard
module DlTypWildcard = IntWildcard
module DlVlanWildcard = NetCore_Wildcard.Make (DlVlanOrderedType)
module DlVlanPcpWildcard = IntWildcard
module NwAddrWildcard = Int32Wildcard
module NwProtoWildcard = IntWildcard
module NwTosWildcard = IntWildcard
module TpPortWildcard = IntWildcard
module PortWildcard = NetCore_Wildcard.Make (PortOrderedType)

type lp = OpenFlow0x01.switchId * port * packet

type ptrn = {
  ptrnDlSrc : dlAddr wildcard;
  ptrnDlDst : dlAddr wildcard;
  ptrnDlType : dlTyp wildcard;
  ptrnDlVlan : dlVlan wildcard;
  ptrnDlVlanPcp : dlVlanPcp wildcard;
  ptrnNwSrc : nwAddr wildcard;
  ptrnNwDst : nwAddr wildcard;
  ptrnNwProto : nwProto wildcard;
  ptrnNwTos : nwTos wildcard;
  ptrnTpSrc : tpPort wildcard;
  ptrnTpDst : tpPort wildcard;
  ptrnInPort : port wildcard
}

type 'a match_modify = ('a * 'a) option

type output = {
  outDlSrc : dlAddr match_modify;
  outDlDst : dlAddr match_modify;
  outDlVlan : dlVlan match_modify;
  outDlVlanPcp : dlVlanPcp match_modify;
  outNwSrc : nwAddr match_modify;
  outNwDst : nwAddr match_modify;
  outNwTos : nwTos match_modify;
  outTpSrc : tpPort match_modify;
  outTpDst : tpPort match_modify;
  outPort : port 
}

type get_packet_handler = 
    OpenFlow0x01.switchId -> port -> packet -> action

  (* Packet count -> Byte count -> unit. *)
and get_count_handler = Int64.t -> Int64.t -> unit

and action_atom =
  | SwitchAction of output
  | ControllerAction of get_packet_handler
  | ControllerQuery of int * get_count_handler

and action = action_atom list

type pred =
  | PrHdr of ptrn
  | PrOnSwitch of OpenFlow0x01.switchId
  | PrOr of pred * pred
  | PrAnd of pred * pred
  | PrNot of pred
  | PrAll
  | PrNone

type pol =
  | PoAction of action
  | PoFilter of pred
  | PoUnion of pol * pol
  | PoSeq of pol * pol
  | PoITE of pred * pol * pol

type payload = 
  | Buf of OpenFlow0x01.bufferId
  | Data of bytes

type value =
  | Pkt of OpenFlow0x01.switchId * port * packet * payload
