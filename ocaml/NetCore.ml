open Wildcard
open Pattern
open MessagesDef
open Packet
open Platform
open NetCoreSemantics
open MonadicController

type get_packet_handler = switchId -> portId -> packet -> unit

type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | None
  | DlSrc of Int64.t
  | DlDst of Int64.t
  (* TODO(arjun): fill in others *)

type action =
  | To of int
  | ToAll
  | GetPacket of get_packet_handler

type policy =
  | Pol of predicate * action list
  | Par of policy * policy (** parallel composition *)


module Make (Platform : PLATFORM) = struct

  let next_id : int ref = ref 0

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = 
    Hashtbl.create 200

  module Handlers : HANDLERS = struct
      
    let get_packet_handler queryId switchId portId packet = 
      (Hashtbl.find get_pkt_handlers queryId) switchId portId packet

  end
          
  let desugar_act act = match act with
    | To pt -> Forward (PhysicalPort pt)
    | ToAll -> Forward AllPorts
    | GetPacket handler ->
      let id = !next_id in
      incr next_id;
      Hashtbl.add get_pkt_handlers id handler;
      ActGetPkt id

  let rec desugar_pred pred = match pred with
    | And (p1, p2) -> 
      PrNot (PrOr (PrNot (desugar_pred p1), PrNot (desugar_pred p2)))
    | Or (p1, p2) ->
      PrOr (desugar_pred p1, desugar_pred p2)
    | Not p -> PrNot (desugar_pred p)
    | All -> PrAll
    | None -> PrNone
    | DlSrc n -> PrHdr { 
      Pattern.all with ptrnDlSrc = WildcardExact n
    }
    | DlDst n -> PrHdr { 
      Pattern.all with ptrnDlDst = WildcardExact n
    }

  let rec desugar_pol pol = match pol with
    | Pol (pred, acts) -> 
      PoAtom (desugar_pred pred, List.map desugar_act acts)
    | Par (pol1, pol2) ->
      PoUnion (desugar_pol pol1, desugar_pol pol2)

  module Controller = MakeDynamic (Platform) (Handlers)

  let clear_handlers () : unit = 
    Hashtbl.clear get_pkt_handlers;
    next_id := 0

  let start_controller (pol : policy Lwt_stream.t) : unit = 
    Controller.start_controller
      (Lwt_stream.map 
         (fun pol -> clear_handlers (); desugar_pol pol)
         pol)

end
