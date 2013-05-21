open Packet
open Misc
open List
open Word

module Internal : sig
  type pred =
  | PrHdr of Pattern.t
  | PrOnSwitch of OpenFlow0x01.switchId
  | PrOr of pred * pred
  | PrAnd of pred * pred
  | PrNot of pred
  | PrAll
  | PrNone

  type pol =
  | PoAction of Action.Output.t
  | PoFilter of pred
  | PoUnion of pol * pol
  | PoSeq of pol * pol

  type payload = 
  | Buf of OpenFlow0x01.bufferId
  | Data of bytes 

  type value =
  | Pkt of OpenFlow0x01.switchId * Pattern.port * packet * payload

  val match_pred : pred -> OpenFlow0x01.switchId 
    -> Pattern.port -> packet -> bool

  val classify : pol -> value -> value list

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
  | To of int
  | ToAll
  | UpdateDlSrc of Int64.t * Int64.t
  | UpdateDlDst of Int64.t * Int64.t
  | UpdateDlVlan of int option * int option (** 12-bits *)
  | GetPacket of get_packet_handler
      
  type policy =
  | Empty
  | Act of action
  | Par of policy * policy (** parallel composition *)
  | Seq of policy * policy
  | Filter of predicate
  | Slice of predicate * policy * predicate

  val par : policy list -> policy
      
  val predicate_to_string : predicate -> string
        
  val action_to_string : action -> string
        
  val policy_to_string : policy -> string

end

val desugar : 
  (unit -> int) -> 
  (unit -> int option) -> 
  External.policy -> 
  (int, External.get_packet_handler) Hashtbl.t -> 
  Internal.pol
