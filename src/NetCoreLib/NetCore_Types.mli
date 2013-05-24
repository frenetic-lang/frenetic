open Packet
open List

module Internal : sig

  type port =
    | Physical of portId
    | All
    | Bucket of int
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

  type ptrn = {
    ptrnDlSrc : DlAddrWildcard.t;
    ptrnDlDst : DlAddrWildcard.t;
    ptrnDlType : DlTypWildcard.t;
    ptrnDlVlan : DlVlanWildcard.t;
    ptrnDlVlanPcp : DlVlanPcpWildcard.t;
    ptrnNwSrc : NwAddrWildcard.t;
    ptrnNwDst : NwAddrWildcard.t;
    ptrnNwProto : NwProtoWildcard.t;
    ptrnNwTos : NwTosWildcard.t;
    ptrnTpSrc : TpPortWildcard.t;
    ptrnTpDst : TpPortWildcard.t;
    ptrnInPort : PortWildcard.t
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

  type action = output list

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

  val port_to_string : port -> string

  val pred_to_string : pred -> string

  val pol_to_string : pol -> string

  val value_to_string : value -> string 
end

module External : sig

  type get_packet_handler = 
    OpenFlow0x01.switchId -> portId -> packet -> unit

  type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of OpenFlow0x01.switchId
  | InPort of portId
  | DlSrc of Int64.t
  | DlDst of Int64.t
  | DlVlan of int option (** 12-bits *)
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

  type action =
  | Pass
  | Drop
  | To of int
  | ToAll
  | UpdateDlSrc of Int64.t * Int64.t
  | UpdateDlDst of Int64.t * Int64.t
  | UpdateDlVlan of int option * int option (** 12-bits *)
  | UpdateSrcIP of Int32.t * Int32.t
  | UpdateDstIP of Int32.t * Int32.t
  | UpdateSrcPort of int * int
  | UpdateDstPort of int * int
  | GetPacket of get_packet_handler
      
  type policy =
  | Empty
  | Act of action
  | Par of policy * policy (** parallel composition *)
  | Seq of policy * policy
  | Filter of predicate
  | Slice of predicate * policy * predicate
  | ITE of predicate * policy * policy

  val par : policy list -> policy
      
  val predicate_to_string : predicate -> string
        
  val action_to_string : action -> string
        
  val policy_to_string : policy -> string

end
