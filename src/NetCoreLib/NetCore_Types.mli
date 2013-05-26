open Packet
open List

module Internal : sig

  type 'a wildcard = 'a NetCore_Wildcard.wildcard

  type port =
    | Physical of portId
    | All
    | Bucket of int
    | Here

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

  (* Duplicates External.  Eventually, External should not depend on Internal.
  *)
  type get_packet_handler = 
      OpenFlow0x01.switchId -> port -> packet -> action

  and get_count_handler = OpenFlow0x01.switchId -> Int64.t -> unit

  and action_atom =
    | SwitchAction of output
    | ControllerAction of get_packet_handler
    | ControllerPacketQuery of int * get_count_handler
    | ControllerByteQuery of int * get_count_handler

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

  val port_to_string : port -> string

  val pred_to_string : pred -> string

  val pol_to_string : pol -> string

  val value_to_string : value -> string 
end

module External : sig

  type get_packet_handler = 
    OpenFlow0x01.switchId -> Internal.port -> packet -> Internal.action
  type get_count_handler = OpenFlow0x01.switchId -> Int64.t -> unit

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
  | DlTyp of int
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
  | GetPacketCount of int * get_count_handler
  | GetByteCount of int * get_count_handler
      
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
