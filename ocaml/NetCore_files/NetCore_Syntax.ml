open OpenFlow0x01Types
open Misc
open Packet.Types

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

let par (pols : policy list) : policy = 
  match pols with
    | x :: xs -> List.fold_right (fun x y -> Par (x, y)) xs x
    | [] -> Empty

let rec predicate_to_string pred = match pred with
  | And (p1,p2) -> Printf.sprintf "(And %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Or (p1,p2) -> Printf.sprintf "(Or %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Not p1 -> Printf.sprintf "(Not %s)" (predicate_to_string p1)
  | NoPackets -> "None"
  | Switch sw -> Printf.sprintf "(Switch %Ld)" sw
  | InPort pt -> Printf.sprintf "(InPort %d)" pt
  | DlSrc add -> Printf.sprintf "(DlSrc %s)" (string_of_mac add)
  | DlDst add -> Printf.sprintf "(DlDst %s)" (string_of_mac add)
  | All -> "All"
  | TcpSrcPort n ->
    Printf.sprintf "(TcpSrcPort %d)" n
  | TcpDstPort n ->
    Printf.sprintf "(TcpDstPort %d)" n
  | SrcIP n ->
    Printf.sprintf "(SrcIP %ld)" n
  | DstIP n ->
    Printf.sprintf "(DstIP %ld)" n
  
let action_to_string act = match act with
  | To pt -> Printf.sprintf "To %d" pt
  | ToAll -> "ToAll"
  | GetPacket _ -> "GetPacket"

let rec policy_to_string pol = match pol with
  | Pol (pred,acts) -> Printf.sprintf "(%s => [%s])" (predicate_to_string pred) (String.concat ";" (List.map action_to_string acts))
  | Par (p1,p2) -> Printf.sprintf "(Union %s %s)" (policy_to_string p1) (policy_to_string p2)
  | Restrict (p1,p2) -> Printf.sprintf "(restrict %s %s)" (policy_to_string p1) (predicate_to_string p2)
  | Empty -> "Empty"


let desugar_policy 
  (pol : policy) 
  (get_pkt_handlers : (int, get_packet_handler) Hashtbl.t) =
  let open Wildcard in
  let open Pattern in
  let open NetCoreEval in
  let next_id = ref 0 in
  let desugar_act act = match act with
    | To pt -> Forward (unmodified, PhysicalPort pt)
    | ToAll -> Forward (unmodified, AllPorts)
    | GetPacket handler ->
      let id = !next_id in
      incr next_id;
      Hashtbl.add get_pkt_handlers id handler;
      ActGetPkt id in
  let rec desugar_pred pred = match pred with
    | And (p1, p2) -> 
      PrAnd (desugar_pred p1, desugar_pred p2)
    | Or (p1, p2) ->
      PrOr (desugar_pred p1, desugar_pred p2)
    | Not p -> PrNot (desugar_pred p)
    | All -> PrAll
    | NoPackets -> PrNone
    | Switch swId -> PrOnSwitch swId
    | InPort pt -> PrHdr (Pattern.inPort pt)
    | DlSrc n -> PrHdr (Pattern.dlSrc n)
    | DlDst n -> PrHdr (Pattern.dlDst n)
    | SrcIP n -> PrHdr (Pattern.ipSrc n)
    | DstIP n -> PrHdr (Pattern.ipDst n)
    | TcpSrcPort n -> PrHdr (Pattern.tcpSrcPort n)
    | TcpDstPort n -> PrHdr (Pattern.tcpDstPort n) in
  let rec desugar_pol pol pred = match pol with
    | Pol (pred', acts) -> 
      PoAtom (desugar_pred (And (pred', pred)), List.map desugar_act acts)
    | Par (pol1, pol2) ->
      PoUnion (desugar_pol pol1 pred, desugar_pol pol2 pred)
    | Restrict (p1, pr1) ->
      desugar_pol p1 (And (pred, pr1)) 
    | Empty -> PoAtom (PrNone, []) in
  Hashtbl.clear get_pkt_handlers;
  desugar_pol pol All
