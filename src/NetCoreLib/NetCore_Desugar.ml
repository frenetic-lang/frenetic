open List
open Packet
open Format
open NetCore_Types

  type get_packet_handler = 
      OpenFlow0x01.switchId -> NetCore_Types.port -> packet -> NetCore_Types.action
  type get_count_handler = Int64.t -> Int64.t -> unit

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
  | DlTyp of int
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

  type action =
  | Pass
  | Drop
  | To of int
  | ToAll
  | UpdateDlSrc of Int64.t * Int64.t
  | UpdateDlDst of Int64.t * Int64.t
  | UpdateDlVlan of int option * int option (** 12-bits *)
  | UpdateSrcIP of Int32.t * Int32.t
  | UpdateDstIP of Int32.t * Int32.t
  | UpdateSrcPort of int * int
  | UpdateDstPort of int * int
  | GetPacket of get_packet_handler
  | GetCount of float * get_count_handler
      
  type policy =
  | Empty
  | Act of action
  | Par of policy * policy (** parallel composition *)
  | Seq of policy * policy
  | Filter of predicate
  | Slice of predicate * policy * predicate
  | ITE of predicate * policy * policy

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
    | DlTyp typ -> 
      Printf.sprintf "(DlTyp %d)" typ
    | DlVlan no -> 
      Printf.sprintf "(DlVlan %s)" (match no with None -> "None" | Some n -> "Some " ^ string_of_int n)
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
    | Pass -> "Pass"
    | Drop -> "Drop"
    | To pt -> 
      Printf.sprintf "To %d" pt
    | ToAll -> "ToAll"
    | UpdateDlSrc(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlDst(old,new0) -> 
      Printf.sprintf "UpdateDlSrc(%Ld,%Ld)" old new0
    | UpdateDlVlan(old,new0) -> 
      Printf.sprintf "UpdateDlSrc (%s,%s)"
        (Packet.dlVlan_to_string old) (Packet.dlVlan_to_string new0)
   | UpdateSrcIP (old, new_) ->
     Printf.sprintf "UpdateSrcIP (%s, %s)"
       (Int32.to_string old) (Int32.to_string new_)
   | UpdateDstIP (old, new_) ->
     Printf.sprintf "UpdateDstIP (%s, %s)"
       (Int32.to_string old) (Int32.to_string new_)
   | UpdateSrcPort (old, new_) ->
     Printf.sprintf "UpdateSrcPort (%s, %s)"
       (string_of_int old) (string_of_int new_)
   | UpdateDstPort (old, new_) ->
     Printf.sprintf "UpdateDstPort (%s, %s)"
       (string_of_int old) (string_of_int new_)
    | GetPacket _ -> 
      Printf.sprintf "GetPacket <fun>"
    | GetCount (time, _) ->
      Printf.sprintf "GetCount %f <fun>" time
        
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
    | ITE (pred, then_pol, else_pol) ->
      Printf.sprintf "ITE (%s, %s, %s)"
        (predicate_to_string pred)
        (policy_to_string then_pol)
        (policy_to_string else_pol)
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
        (false, false) 
      | _ -> 
        failwith "Not yet implemented" in 
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
      | DlTyp _ -> false
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
      | ITE _ -> 
        failwith "Not yet implemented"
    in
    let sliceB, vlanB = check_pol pol in
    if sliceB && vlanB 
    then failwith ("Error: policy contains slices and also matches on or " ^
                      "modifies VLANs.")
    else ()


let desugar (genvlan : unit -> int option) (pol : policy) : NetCore_Types.pol =
  let desugar_act act =
    match act with
    | Pass -> NetCore_Action.Output.pass
    | Drop -> NetCore_Action.Output.drop
    | To pt -> 
      NetCore_Action.Output.forward pt
    | ToAll -> 
      NetCore_Action.Output.to_all
    | UpdateDlSrc(old,new0) -> 
      NetCore_Action.Output.updateDlSrc old new0
    | UpdateDlDst(old,new0) -> 
      NetCore_Action.Output.updateDlDst old new0
    | UpdateDlVlan(old,new0) -> 
      NetCore_Action.Output.updateDlVlan old new0
    | UpdateSrcIP (old, new_) ->
      NetCore_Action.Output.updateSrcIP old new_
    | UpdateDstIP (old, new_) ->
      NetCore_Action.Output.updateDstIP old new_
    | UpdateSrcPort (old, new_) ->
      NetCore_Action.Output.updateSrcPort old new_
    | UpdateDstPort (old, new_) ->
      NetCore_Action.Output.updateDstPort old new_
    | GetPacket handler ->
      NetCore_Action.Output.controller handler
    | GetCount (time, handler) ->
      NetCore_Action.Output.query time handler
    in
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
      PrHdr (NetCore_Pattern.inPort (Physical pt))
    | DlSrc n -> 
      PrHdr (NetCore_Pattern.dlSrc n)
    | DlDst n -> 
      PrHdr (NetCore_Pattern.dlDst n)
    | DlVlan n -> 
      PrHdr (NetCore_Pattern.dlVlan n)
    | DlTyp n ->
      PrHdr (NetCore_Pattern.dlType n)
    | SrcIP n -> 
      PrHdr (NetCore_Pattern.ipSrc n)
    | DstIP n -> 
      PrHdr (NetCore_Pattern.ipDst n)
    | TcpSrcPort n -> 
      PrHdr (NetCore_Pattern.tcpSrcPort n)
    | TcpDstPort n -> 
      PrHdr (NetCore_Pattern.tcpDstPort n) in
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
    | ITE (pred, then_pol, else_pol) ->
      let (then_po, slice1) = desugar_pol curr then_pol in
      let (else_po, slice2) = desugar_pol curr else_pol in
      (PoITE (desugar_pred pred, then_po, else_po), 
       slice1 @ slice2)
    | Empty -> 
      let pol' = PoFilter PrNone in 
      let slice' = [] in 
      (pol', slice') 
    | Slice(sin, spol, sout) -> 

      (* Informal slicing strategy:
       *
       *     {in} P {out} -->
       *
       *     (curr && in; set vlan := next) | next || rec; 
       *     P;
       *     (next && out; set vlan := curr) | -(next && out)
       *)

      let next = genvlan () in 
      let sin' = desugar_pred sin in 
      let sout' = desugar_pred sout in 
      let spol',sslice' = desugar_pol next spol in 
      let pred_rec = 
        if List.length sslice' > 0 then
          List.fold_left 
            (fun acc s -> PrAnd(acc, PrHdr(NetCore_Pattern.dlVlan s)))
            PrAll sslice'
        else 
          PrNone in
      let pred_curr = PrHdr(NetCore_Pattern.dlVlan(curr)) in 
      let pred_next = PrHdr(NetCore_Pattern.dlVlan(next)) in 
      let pol1' = 
        PoUnion ( PoSeq ( PoFilter(PrAnd(pred_curr, sin'))
                        , PoAction (NetCore_Action.Output.updateDlVlan curr next))
                , PoFilter (PrOr (pred_next, pred_rec))) in 
      let pol2' = spol' in 
      let pol3' = 
        PoUnion ( PoSeq ( PoFilter(PrAnd(pred_next, sout'))
                        , PoAction (NetCore_Action.Output.updateDlVlan next curr))
                , PoFilter(PrNot(PrAnd(pred_next, sout')))) in 
      let pol' = PoSeq(pol1', PoSeq(pol2', pol3')) in 
      let slice' = next::sslice' in 
      (pol', slice') in 
  fst (desugar_pol None pol)
