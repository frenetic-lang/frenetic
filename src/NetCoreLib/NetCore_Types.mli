open Packet
open List

module Internal : sig

  type 'a wildcard = 'a NetCore_Wildcard.wildcard

  type port =
    | Physical of portId
    | All
    | Here

  module DlAddrWildcard : NetCore_Wildcard.Wildcard with type a = dlAddr
  module DlTypWildcard : NetCore_Wildcard.Wildcard with type a = dlTyp
  module DlVlanWildcard : NetCore_Wildcard.Wildcard with type a = dlVlan
  module DlVlanPcpWildcard : NetCore_Wildcard.Wildcard with type a = dlVlanPcp
  module NwAddrWildcard : NetCore_Wildcard.Wildcard with type a = nwAddr
  module NwProtoWildcard : NetCore_Wildcard.Wildcard with type a = nwProto
  module NwTosWildcard : NetCore_Wildcard.Wildcard with type a = nwTos
  module TpPortWildcard : NetCore_Wildcard.Wildcard with type a = tpPort
  module PortWildcard : NetCore_Wildcard.Wildcard with type a = port

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

  (** Note that OpenFlow does not allow the [dlType] and [nwProto]
      fields to be modified. *)
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

end
