(** The NetCore policy language *)
open OpenFlow0x01.Types
open Packet.Types

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

module Make : functor (Platform : OpenFlow0x01.PLATFORM) -> sig
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

module Featherweight : sig

  module type POLICY = sig
    val policy : Syntax.policy
    (* Necessary due to static compilation in FwOF. *)
    val switches : switchId list
  end


  module Make (Platform : OpenFlow0x01.PLATFORM) 
    (Policy : POLICY) : sig

      type state

      val init_packet_out : unit -> state
      val init_flow_mod : unit -> state
        
      (* Returns after expected switches connect, but still processes messages
         in an asynchronous thread. *)
      val start : state -> unit Lwt.t
    end

end

module Z3 : sig

(* Switch, Port, DlDst, DlSrc *)
  type zPacket = 
      ZPacket of Int64.t * int * Int64.t * Int64.t

  type zVar = 
      string

  type zSort = 
    | SPacket
    | SInt
    | SFunction of zSort * zSort
    | SRelation of zSort list

  type zTerm = 
    | TVar of zVar
    | TPacket of zPacket
    | TInt of Int64.t
    | TFunction of zVar * zTerm list

  type zAtom =
    | ZTrue
    | ZFalse 
    | ZNot of zAtom
    | ZEquals of zTerm * zTerm
    | ZRelation of zVar * zTerm list
        
  type zRule =
    | ZRule of zVar * zVar list * zAtom list

  type zDeclaration = 
    | ZVarDeclare of zVar * zSort
    | ZSortDeclare of zVar * (zVar * (zVar * zSort) list) list 
    | ZFunDeclare of zVar * zVar * zSort * zSort * string

  type zProgram = 
    | ZProgram of zRule list * zVar


  val fresh : zSort -> zVar
  val solve : zProgram -> string

  module Topology : sig

    type link = Link of OpenFlow0x01.Types.switchId * Packet.Types.portId

    type topology = Topology of (link * link) list

    val bidirectionalize : topology -> topology

  end

end
