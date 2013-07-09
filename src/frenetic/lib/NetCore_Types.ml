open List
open Packet
open Format

type switchId = int64
type portId = int32

let string_of_portId pid = Printf.sprintf "%ld" pid

type 'a wildcard =
  | WildcardExact of 'a
  | WildcardAll
  | WildcardNone

type port =
  | Physical of portId
  | All
  | Here

type lp = switchId * port * packet

type ptrn = {
  ptrnDlSrc : dlAddr wildcard;
  ptrnDlDst : dlAddr wildcard;
  ptrnDlTyp : dlTyp wildcard;
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

let id = 
  { outDlSrc = None;
    outDlDst = None;
    outDlVlan = None;
    outDlVlanPcp = None;
    outNwSrc = None;
    outNwDst = None;
    outNwTos = None;
    outTpSrc = None;
    outTpDst = None;
    outPort = Here }

type get_packet_handler = switchId -> port -> packet -> action

  (* Packet count -> Byte count -> unit. *)
and get_count_handler = Int64.t -> Int64.t -> unit

and action_atom =
  | SwitchAction of output
  | ControllerAction of get_packet_handler
  | ControllerQuery of float * get_count_handler

and action = action_atom list

type pred =
  | Hdr of ptrn
  | OnSwitch of switchId
  | Or of pred * pred
  | And of pred * pred
  | Not of pred
  | Everything
  | Nothing

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; 
		      ip_reasm : bool; queue_stats : bool; 
		      port_blocked : bool }

type switchFeatures = { datapath_id : switchId; num_buffers : int;
			num_tables : int; supported_capabilities : capabilities;
			ports : portId list }

type switchEvent =
  | SwitchUp of switchId * switchFeatures
  | SwitchDown of switchId

type pol =
  | HandleSwitchEvent of (switchEvent -> unit)
  | Action of action
  | ActionChoice of action list
  | Filter of pred
  | Union of pol * pol
  | Seq of pol * pol
  | ITE of pred * pol * pol


type value =
  | Pkt of switchId * port * packet * OpenFlow0x01.Payload.t

let all = {
  ptrnDlSrc = WildcardAll;
  ptrnDlDst = WildcardAll;
  ptrnDlTyp = WildcardAll;
  ptrnDlVlan = WildcardAll;
  ptrnDlVlanPcp = WildcardAll;
  ptrnNwSrc = WildcardAll;
  ptrnNwDst = WildcardAll;
  ptrnNwProto = WildcardAll;
  ptrnNwTos = WildcardAll;
  ptrnTpSrc = WildcardAll;
  ptrnTpDst = WildcardAll;
  ptrnInPort = WildcardAll
}

let empty = {
  ptrnDlSrc = WildcardNone;
  ptrnDlDst = WildcardNone;
  ptrnDlTyp = WildcardNone;
  ptrnDlVlan = WildcardNone;
  ptrnDlVlanPcp = WildcardNone;
  ptrnNwSrc = WildcardNone;
  ptrnNwDst = WildcardNone;
  ptrnNwProto = WildcardNone;
  ptrnNwTos = WildcardNone;
  ptrnTpSrc = WildcardNone;
  ptrnTpDst = WildcardNone;
  ptrnInPort = WildcardNone
}

let inPort pt =
  { all with ptrnInPort = WildcardExact pt }

let dlSrc mac =
  { all with ptrnDlSrc = WildcardExact mac }

let dlDst mac =
  { all with ptrnDlDst = WildcardExact mac }

let dlTyp typ =
  { all with ptrnDlTyp = WildcardExact typ }

let dlVlan vlan =
  { all with ptrnDlVlan = WildcardExact vlan }

let dlVlanPcp pcp =
  { all with ptrnDlVlanPcp = WildcardExact pcp }

let ipSrc ip =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwSrc = WildcardExact ip }

let ipDst ip =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwDst = WildcardExact ip }

let ipProto proto =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwProto = WildcardExact proto }

let ipTos tos =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwTos = WildcardExact tos }

let tpSrcPort proto tpPort =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwProto = WildcardExact proto;
    ptrnTpSrc = WildcardExact tpPort }

let tpDstPort proto tpPort =
  { all with
    ptrnDlTyp = WildcardExact 0x800;
    ptrnNwProto = WildcardExact proto;
    ptrnTpDst = WildcardExact tpPort }

let tcpSrcPort = tpSrcPort 6
let tcpDstPort = tpDstPort 6
let udpSrcPort = tpSrcPort 17
let udpDstPort = tpDstPort 17
