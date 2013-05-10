open OpenFlow0x01Types
open Misc
open Packet.Types

module Action = NetCoreAction.NetCoreAction
module Pattern = Action.Pattern
module Port = NetCoreAction.Port

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
  | DlVlan of int (** 16-bits *)
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

type action =
  | To of int
  | ToAll
  | GetPacket of get_packet_handler

type policy =
  | Empty
  | Act of action
  | Par of policy * policy (** parallel composition *)
  | Seq of policy * policy
  | Filter of predicate
  | Slice of predicate * policy * predicate

let par (pols : policy list) : policy = 
  match pols with
    | x :: xs -> List.fold_right (fun x y -> Par (x, y)) xs x
    | [] -> Empty

let rec predicate_to_string pred = match pred with
  | And (p1,p2) -> 
    Printf.sprintf "(And %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Or (p1,p2) -> 
    Printf.sprintf "(Or %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Not p1 -> 
    Printf.sprintf "(Not %s)" (predicate_to_string p1)
  | NoPackets -> 
    Printf.sprintf "None"
  | Switch sw -> 
    Printf.sprintf "(Switch %Ld)" sw
  | InPort pt -> 
    Printf.sprintf "(InPort %d)" pt
  | DlSrc add -> 
    Printf.sprintf "(DlSrc %s)" (string_of_mac add)
  | DlDst add -> 
    Printf.sprintf "(DlDst %s)" (string_of_mac add)
  | DlVlan n -> 
    Printf.sprintf "(DlVlan %d)" n
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

let rec policy_to_string pol = "POLICY"
(*
  | 
  | Act (pred,acts) -> Printf.sprintf "(%s => [%s])" (predicate_to_string pred) (String.concat ";" (List.map action_to_string acts))
  | Par (p1,p2) -> Printf.sprintf "(Union %s %s)" (policy_to_string p1) (policy_to_string p2)
  | Seq (p1,p2) -> Printf.sprintf "(Seq %s %s)" (policy_to_string p1) (policy_to_string p2)
  | Restrict (p1,p2) -> Printf.sprintf "(restrict %s %s)" (policy_to_string p1) (predicate_to_string p2)
  | Empty -> "Empty"
*)

(* Fail if a policy contains slices and also matches or sets VLANs. *)
let check_policy_vlans (pol : policy) : unit = 
  (* Apparently we don't have modifications yet, so this will always succeed. *)
  let check_act (act : action) = 
    match act with
    | To _ -> 
      (false, false)
    | ToAll -> 
      (false, false)
    | GetPacket _ -> 
      (false, false) in 
  (* And we don't have VLANs yet, so this whole check is pretty useless 
   * right now. *)
  let rec check_pred (pred : predicate) =
    match pred with
    | And (pr1, pr2) -> check_pred pr1 || check_pred pr2
    | Or (pr1, pr2) -> check_pred pr1 || check_pred pr2
    | Not pr -> check_pred pr
    | All -> false
    | NoPackets -> false
    | Switch _ -> false
    | InPort _ -> false
    | DlSrc _ -> false
    | DlDst _ -> false
    | DlVlan _ -> true
    | SrcIP _ -> false
    | DstIP _ -> false
    | TcpSrcPort _ -> false
    | TcpDstPort _ -> false in 
  let rec check_pol (pol : policy) = 
    match pol with
    | Act act -> check_act act
    | Par (p1, p2) -> 
      let sliceB, vlanB = check_pol p1 in
      let sliceB', vlanB' = check_pol p2 in
      (sliceB || sliceB', vlanB || vlanB')
    | Seq (p1, p2) ->
      let sliceB, vlanB = check_pol p1 in
      let sliceB', vlanB' = check_pol p2 in
      (sliceB || sliceB', vlanB || vlanB')
    | Filter pred -> (false, check_pred pred)
    | Empty -> (false, false)
    | Slice (ingress, pol', egress) ->
      let vlanB = check_pred ingress in
      let _, vlanB' = check_pol pol' in
      let vlanB'' = check_pred egress in
      (true, vlanB || vlanB' || vlanB'')
    in
  let sliceB, vlanB = check_pol pol in
  if sliceB && vlanB 
  then failwith ("Error: policy contains slices and also matches on or " ^
                "modifies VLANs.")
  else ()

let desugar 
  (genbucket : unit -> int)
  (genvlan : unit -> int)
  (pol : policy) 
  (get_pkt_handlers : (int, get_packet_handler) Hashtbl.t) =
  let open NetCoreEval in 
  let desugar_act act = 
    match act with
    | To pt -> 
      Action.forward pt
    | ToAll ->
      failwith "NYI"
    | GetPacket handler ->
      let id = genbucket () in 
      Hashtbl.add get_pkt_handlers id handler;
      Action.bucket id in
  let rec desugar_pred pred = match pred with
    | And (p1, p2) -> 
      PrAnd (desugar_pred p1, desugar_pred p2)
    | Or (p1, p2) ->
      PrOr (desugar_pred p1, desugar_pred p2)
    | Not p -> PrNot (desugar_pred p)
    | All -> PrAll
    | NoPackets -> PrNone
    | Switch swId -> PrOnSwitch swId
    | InPort pt -> PrHdr (Pattern.inPort (Port.Physical pt))
    | DlSrc n -> PrHdr (Pattern.dlSrc n)
    | DlDst n -> PrHdr (Pattern.dlDst n)
    | DlVlan n -> PrHdr (Pattern.dlVlan n)
    | SrcIP n -> PrHdr (Pattern.ipSrc n)
    | DstIP n -> PrHdr (Pattern.ipDst n)
    | TcpSrcPort n -> PrHdr (Pattern.tcpSrcPort n)
    | TcpDstPort n -> PrHdr (Pattern.tcpDstPort n) in
  let rec desugar_pol curr pol = 
    match pol with
    | Act action -> 
      let pol' = PoAction (desugar_act action) in 
      let slice' = [] in 
      (pol', slice')
    | Filter pred -> 
      let pol' = PoFilter (desugar_pred pred) in 
      let slice' = [] in
      (pol', slice')
    | Par (pol1, pol2) ->
      let pol1',slice1 = desugar_pol curr pol1 in  
      let pol2',slice2 = desugar_pol curr pol2 in  
      let pol' = PoUnion (pol1', pol2') in 
      let slice' = slice1 @ slice2 in 
      (pol', slice')
    | Seq (pol1, pol2) ->
      let pol1',slice1 = desugar_pol curr pol1 in  
      let pol2',slice2 = desugar_pol curr pol2 in  
      let pol' = PoSeq (pol1', pol2') in 
      let slice' = slice1 @ slice2 in 
      (pol', slice')
    | Empty -> 
      let pol' = PoFilter PrNone in 
      let slice' = [] in 
      (pol', slice') 
    | Slice(sin, spol, sout) -> 
      let next = genvlan () in 
      let sin' = desugar_pred sin in 
      let sout' = desugar_pred sout in 
      let spol',sslice' = desugar_pol next pol in 
      let pred_rec = 
        List.fold_left 
          (fun acc s -> PrAnd(acc, PrHdr(Pattern.dlVlan s)))
          PrAll sslice' in 
      let pred_curr = PrHdr(Pattern.dlVlan(curr)) in 
      let pred_next = PrHdr(Pattern.dlVlan(next)) in 
      let pol1' = 
        PoUnion(PoSeq(PoFilter(PrAnd(pred_curr, sin')), failwith "tag with next slice"),
                PoFilter(PrOr(pred_next, pred_rec))) in 
      let pol2' = spol' in 
      let pol3' = 
        PoUnion(PoSeq(PoFilter(PrAnd(pred_next, sout')), failwith "tag with curr slice"),
                PoFilter(PrNot(PrAnd(pred_next, sout')))) in 
      let pol' = PoSeq(pol1', PoSeq(pol2', pol3')) in 
      let slice' = next::sslice' in 
      (pol', slice') in 
  Hashtbl.clear get_pkt_handlers;
  fst (desugar_pol 0 pol) 
