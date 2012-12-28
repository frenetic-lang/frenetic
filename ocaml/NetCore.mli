(** The NetCore policy language *)
open MessagesDef
open Packet
open Platform

type get_packet_handler = switchId -> portId -> packet -> unit

(** Predicates match packets based on their header values. 
    
    TODO(arjun): fill in others *)
type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | None
  | DlSrc of Int64.t  (** Match Ethernet source address (48-bits) *)
  | DlDst of Int64.t (** Match Ethernet destination address (48-bits) *)

type action =
  | To of portId (** [To n] sends matching packets to port [n]. *)
  | ToAll (** Send matching packets out of all ports. *)
  | GetPacket of get_packet_handler

type policy =
  | Pol of predicate * action list
  | Par of policy * policy (** parallel composition *)


module Make : functor (Platform : PLATFORM) -> sig
  val set_policy : policy -> unit
  val start_controller : policy -> unit
end

