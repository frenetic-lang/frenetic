(** The NetCore policy language *)
open OpenFlow0x01Types
open OpenFlow0x01.Sig
open Packet

module Syntax : sig

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
    | Empty

  val par : policy list -> policy

  val predicate_to_string : predicate -> string
  val action_to_string : action -> string
  val policy_to_string : policy -> string

(** Desugars the surface syntax policy to the internal (Coq-extracted) policy
    and a hashtable of handlers. *)
  val desugar_policy : 
    policy 
    -> (int, get_packet_handler) Hashtbl.t 
    -> NetCoreEval.pol

end

module Make : functor (Platform : OpenFlow0x01.Sig.PLATFORM) -> sig
  val start_controller : Syntax.policy Lwt_stream.t -> unit Lwt.t
end

module Modules : sig

  module Learning : sig
  
    val learned_hosts : (switchId * WordInterface.Word48.t, portId) Hashtbl.t

    val policy : Syntax.policy Lwt_stream.t

  end 

  module Routing : sig
      
    val policy : Syntax.policy Lwt_stream.t
    
  end

end

