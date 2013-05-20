open OpenFlow0x01Types
open NetworkPacket
open Packet.Types
open Misc
open List
open Word

module Internal = struct
  type pred =
  | PrHdr of Pattern.t
  | PrOnSwitch of switchId
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
  | Buf of bufferId
  | Data of bytes 

  type value =
  | Pkt of switchId * Pattern.port * packet * payload

  let rec match_pred pr sw pt pk =
    match pr with
    | PrHdr pat -> 
      Pattern.match_packet pt pk pat
    | PrOnSwitch sw' -> 
      if Word64.eq_dec sw sw' then true else false
    | PrOr (p1, p2) -> 
      (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrAnd (p1, p2) -> 
      (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrNot p' -> 
      not (match_pred p' sw pt pk)
    | PrAll -> 
      true
    | PrNone -> 
      false

  let serialize_pkt = Packet_Parser.serialize_packet

  let eval_action inp act =
    let Pkt (sw, pt, pk, pay) = inp in
    map (fun (pt',pk') -> Pkt (sw, pt', pk', pay)) (Action.Output.apply_action act (pt, pk))

  let rec classify p inp = match p with 
    | PoAction action -> 
      eval_action inp action
    | PoFilter pred0 ->
      let Pkt (sw, pt, pk, buf) = inp in
      if match_pred pred0 sw pt pk then [inp] else []
    | PoUnion (p1, p2) -> 
      classify p1 inp @ classify p2 inp
    | PoSeq (p1, p2) -> 
      concat_map (classify p2) (classify p1 inp)

  let rec pred_to_string pred = match pred with 
    | PrHdr pat -> 
      Pattern.to_string pat
    | PrOnSwitch sw -> 
      "Switch " ^ (string_of_switchId sw)
    | PrOr (p1,p2) -> 
      (pred_to_string p1) ^ " || " ^ (pred_to_string p2)
    | PrAnd (p1,p2) -> 
      (pred_to_string p1) ^ " && " ^ (pred_to_string p2)
    | PrNot p -> 
      "Not " ^ (pred_to_string p)
    | PrAll -> 
      "*"
    | PrNone -> 
      "None"

  let rec pol_to_string pol =
    let wrap s = "(" ^ s ^ ")" in
    match pol with
    | PoAction a -> 
      Action.Output.to_string a
    | PoFilter pr -> 
      pred_to_string pr
    | PoUnion (p1,p2) -> 
      (wrap (pol_to_string p1)) 
      ^ " | " 
      ^ (wrap (pol_to_string p2))
    | PoSeq (p1,p2) -> 
      (wrap (pol_to_string p1)) 
      ^ "; " 
      ^ (wrap (pol_to_string p2))

  let value_to_string = function 
    | Pkt (sid, port, pkt, pay) ->
      Printf.sprintf "(%s, %s, %s, _)" 
        (string_of_switchId sid) 
        (Pattern.string_of_port port) 
        (packet_to_string pkt)
end

module External = struct
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

  let par (pols : policy list) : policy = match pols with 
    | x :: xs -> 
      List.fold_right (fun x y -> Par (x, y)) xs x
    | [] -> 
      Empty
      
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
      Printf.sprintf "(DlVlan %s)" (string_of_option string_of_int n)
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
    | To pt -> 
      Printf.sprintf "To %d" pt
    | ToAll -> "ToAll"
    | UpdateDlSrc(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlDst(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlVlan(old,new0) -> 
      Printf.sprintf "UpdateDlSrc %s" (string_of_pair NetworkPacket.dlVlan_to_string (old, new0))
    | GetPacket _ -> 
      Printf.sprintf "GetPacket <fun>"
        
  let rec policy_to_string pol = match pol with 
    | Empty -> 
      "Empty"
    | Act act -> 
      Printf.sprintf "%s" (action_to_string act)
    | Par (p1,p2) -> 
      Printf.sprintf "(%s) U (%s)" 
        (policy_to_string p1) 
        (policy_to_string p2)
    | Seq (p1,p2) -> 
      Printf.sprintf "(%s) >> (%s)" 
        (policy_to_string p1) 
        (policy_to_string p2)
    | Filter pr -> 
      predicate_to_string pr
    | Slice (ingress,pol',egress) -> 
      Printf.sprintf "{%s} %s {%s}" 
        (predicate_to_string ingress) 
        (policy_to_string pol') 
        (predicate_to_string egress)

  (* JNF: better comment *)
  (* Fail if a policy contains slices and also matches or sets VLANs. *)
  let check_policy_vlans (pol : policy) : unit = 
    let check_act (act : action) = 
      match act with
      | To _ -> 
        (false, false)
      | ToAll -> 
        (false, false)
      | UpdateDlSrc _ -> 
        (false,false)
      | UpdateDlDst _ -> 
        (false,false)
      | UpdateDlVlan _ -> 
        (false,true)
      | GetPacket _ -> 
        (false, false) in 
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
end

let desugar 
  (genbucket : unit -> int)
  (genvlan : unit -> int option)
  (pol : External.policy) 
  (get_pkt_handlers : (int, External.get_packet_handler) Hashtbl.t) :
  Internal.pol =
  let open Internal in
  let open External in
  let desugar_act act = match act with
    | To pt -> 
      Action.Output.forward pt
    | ToAll -> 
      Action.Output.to_all
    | UpdateDlSrc(old,new0) -> 
      Action.Output.updateDlSrc old new0
    | UpdateDlDst(old,new0) -> 
      Action.Output.updateDlDst old new0
    | UpdateDlVlan(old,new0) -> 
      Action.Output.updateDlVlan old new0
    | GetPacket handler ->
      let id = genbucket () in 
      Hashtbl.add get_pkt_handlers id handler;
      Action.Output.bucket id in
  let rec desugar_pred pred = match pred with
    | And (p1, p2) -> 
      PrAnd (desugar_pred p1, desugar_pred p2)
    | Or (p1, p2) ->
      PrOr (desugar_pred p1, desugar_pred p2)
    | Not p -> PrNot (desugar_pred p)
    | All -> 
      PrAll
    | NoPackets -> 
      PrNone
    | Switch swId -> 
      PrOnSwitch swId
    | InPort pt -> 
      PrHdr (Pattern.inPort (Pattern.Physical pt))
    | DlSrc n -> 
      PrHdr (Pattern.dlSrc n)
    | DlDst n -> 
      PrHdr (Pattern.dlDst n)
    | DlVlan n -> 
      PrHdr (Pattern.dlVlan n)
    | SrcIP n -> 
      PrHdr (Pattern.ipSrc n)
    | DstIP n -> 
      PrHdr (Pattern.ipDst n)
    | TcpSrcPort n -> 
      PrHdr (Pattern.tcpSrcPort n)
    | TcpDstPort n -> 
      PrHdr (Pattern.tcpDstPort n) in
  let rec desugar_pol curr pol = match pol with 
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
      let spol',sslice' = desugar_pol next spol in 
      let pred_rec = 
        List.fold_left 
          (fun acc s -> PrAnd(acc, PrHdr(Pattern.dlVlan s)))
          PrAll sslice' in 
      let pred_curr = PrHdr(Pattern.dlVlan(curr)) in 
      let pred_next = PrHdr(Pattern.dlVlan(next)) in 
      let pol1' = 
        PoUnion(PoSeq(PoFilter(PrAnd(pred_curr, sin')), PoAction (Action.Output.updateDlVlan curr next)),
                PoFilter(PrOr(pred_next, pred_rec))) in 
      let pol2' = spol' in 
      let pol3' = 
        PoUnion(PoSeq(PoFilter(PrAnd(pred_next, sout')), PoAction (Action.Output.updateDlVlan next curr)),
                PoFilter(PrNot(PrAnd(pred_next, sout')))) in 
      let pol' = PoSeq(pol1', PoSeq(pol2', pol3')) in 
      let slice' = next::sslice' in 
      (pol', slice') in 
  Hashtbl.clear get_pkt_handlers;
  fst (desugar_pol None pol)
