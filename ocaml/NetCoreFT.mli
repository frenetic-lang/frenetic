(** The NetCore policy language *)
open Packet
open OpenFlow0x04Types
open Platform0x04

type get_packet_handler = switchId -> portId -> packet -> unit

(** Predicates match packets based on their header values. 
    
    TODO(arjun): fill in others *)
type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of switchId
  | InPort of portId
  | DlType of int (** 8 bits **)
  | DlSrc of Int64.t  (** Match Ethernet source address (48-bits) *)
  | DlDst of Int64.t (** Match Ethernet destination address (48-bits) *)
  | DlVlan of int (** 12 bits **)
  | DlVlanPcp of int (** 3 bit **)
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | MPLS of int
  | TcpSrcPort of int (** 16-bits *)
  | TcpDstPort of int (** 16-bits *)

type action =
  | To of portId (** [To mods n] sends matching packets to port [n]. *)
  | ToAll (** Send matching packets out of all ports. *)
  | Group of groupId
  | GetPacket of get_packet_handler

type policy =
  | Pol of predicate * action list
  | Par of policy * policy (** parallel composition *)
  | Restrict of policy * predicate

val policy_to_string : policy -> string
type group_htbl = (OpenFlow0x04Types.switchId, (int32 * OpenFlow0x04Types.groupType * NetCoreEval0x04.act list list) list) Hashtbl.t

val desugar_act : action -> NetCoreEval0x04.act

val desugar_pred : predicate -> NetCoreEval.pred

val desugar_pol : policy -> NetCoreEval0x04.pol

module Make : functor (Platform : PLATFORM) -> sig
  val start_controller : (policy*group_htbl) Lwt_stream.t -> unit Lwt.t
end

