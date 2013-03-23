open MessagesDef
open Packet

type get_packet_handler = switchId -> portId -> packet -> unit

type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of switchId
  | InPort of portId
  | DlSrc of Int64.t
  | DlDst of Int64.t
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

type action =
  | To of int
  | ToAll
  | GetPacket of get_packet_handler

type policy =
  | Pol of predicate * action list
  | Par of policy * policy (** parallel composition *)
  | Restrict of policy * predicate

val predicate_to_string : predicate -> string
val action_to_string : action -> string
val policy_to_string : policy -> string

(** Desugars the surface syntax policy to the internal (Coq-extracted) policy
    and a hashtable of handlers. *)
val desugar_policy : 
     policy 
  -> (int, get_packet_handler) Hashtbl.t 
  -> NetCoreEval.pol
