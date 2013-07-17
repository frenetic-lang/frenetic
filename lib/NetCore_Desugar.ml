open List
open Packet
open Format
module NCT = NetCore_Types

  type get_packet_handler = 
      OpenFlow0x01.switchId -> NCT.port -> packet -> NCT.action
  type get_count_handler = Int64.t -> Int64.t -> unit

  type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of OpenFlow0x01.switchId
  | InPort of OpenFlow0x01.portId
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
      
  let rec string_of_predicate pred = match pred with
    | And (p1,p2) -> 
      Printf.sprintf "(And %s %s)" (string_of_predicate p1) (string_of_predicate p2)
    | Or (p1,p2) -> 
      Printf.sprintf "(Or %s %s)" (string_of_predicate p1) (string_of_predicate p2)
    | Not p1 -> 
      Printf.sprintf "(Not %s)" (string_of_predicate p1)
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
        
  let string_of_action act = match act with
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
        (Packet.string_of_dlVlan old) (Packet.string_of_dlVlan new0)
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
        
  let rec string_of_policy pol = match pol with 
    | Empty -> 
      "Empty"
    | Act act -> 
      Printf.sprintf "%s" (string_of_action act)
    | Par (p1,p2) -> 
      Printf.sprintf "(%s) U (%s)" 
        (string_of_policy p1) 
        (string_of_policy p2)
    | Seq (p1,p2) -> 
      Printf.sprintf "(%s) >> (%s)" 
        (string_of_policy p1) 
        (string_of_policy p2)
    | Filter pr -> 
      string_of_predicate pr
    | ITE (pred, then_pol, else_pol) ->
      Printf.sprintf "ITE (%s, %s, %s)"
        (string_of_predicate pred)
        (string_of_policy then_pol)
        (string_of_policy else_pol)
    | Slice (ingress,pol',egress) -> 
      Printf.sprintf "{%s} %s {%s}" 
        (string_of_predicate ingress) 
        (string_of_policy pol') 
        (string_of_predicate egress)

  (* JNF: better comment *)
  (* Fail if a policy contains slices and also matches or sets VLANs. *)
  let check_policy_vlans (pol : policy) : unit = 
    let check_act (act : action) = 
      match act with
      | UpdateDlVlan _ -> 
        (false,true)
      | Pass
      | Drop
      | To _
      | ToAll
      | UpdateDlSrc _
      | UpdateDlDst _
      | UpdateSrcIP _
      | UpdateDstIP _ 
      | UpdateSrcPort _ 
      | UpdateDstPort _ 
      | GetPacket _
      | GetCount _ ->
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
      | ITE (pr, thn, els) -> 
        let vlanIf = check_pred pr in
        let sliceThen, vlanThen = check_pol thn in
        let sliceElse, vlanElse = check_pol els in
        (sliceThen || sliceElse, vlanIf || vlanThen || vlanElse)
    in
    let sliceB, vlanB = check_pol pol in
    if sliceB && vlanB 
    then failwith ("Error: policy contains slices and also matches on or " ^
                      "modifies VLANs.")
    else ()


let desugar (genvlan : unit -> int option) (pol : policy) : NCT.pol =
  let desugar_act act =
    match act with
    | Pass -> NetCore_Action.Output.pass
    | Drop -> NetCore_Action.Output.drop
    | To pt -> 
      NetCore_Action.Output.forward (Int32.of_int pt)
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
  let rec desugar_pred (pred : predicate) : NCT.pred = 
    match pred with
    | And (p1, p2) -> 
      NCT.And (desugar_pred p1, desugar_pred p2)
    | Or (p1, p2) ->
      NCT.Or (desugar_pred p1, desugar_pred p2)
    | Not p -> NCT.Not (desugar_pred p)
    | All -> 
      NCT.Everything
    | NoPackets -> 
      NCT.Nothing
    | Switch swId -> 
      NCT.OnSwitch swId
    | InPort pt -> 
      NCT.Hdr (NCT.inPort (NCT.Physical (Int32.of_int pt)))
    | DlSrc n -> 
      NCT.Hdr (NCT.dlSrc n)
    | DlDst n -> 
      NCT.Hdr (NCT.dlDst n)
    | DlVlan n -> 
      NCT.Hdr (NCT.dlVlan n)
    | DlTyp n ->
      NCT.Hdr (NCT.dlTyp n)
    | SrcIP n -> 
      NCT.Hdr (NCT.ipSrc n)
    | DstIP n -> 
      NCT.Hdr (NCT.ipDst n)
    | TcpSrcPort n -> 
      NCT.Hdr (NCT.tcpSrcPort n)
    | TcpDstPort n -> 
      NCT.Hdr (NCT.tcpDstPort n) in
  let rec desugar_pol curr pol = match pol with 
    | Act action -> 
      let pol' = NCT.Action (desugar_act action) in 
      let slice' = [] in 
      (pol', slice')
    | Filter pred -> 
      let pol' = NCT.Filter (desugar_pred pred) in 
      let slice' = [] in
      (pol', slice')
    | Par (pol1, pol2) ->
      let pol1',slice1 = desugar_pol curr pol1 in  
      let pol2',slice2 = desugar_pol curr pol2 in  
      let pol' = NCT.Union (pol1', pol2') in 
      let slice' = slice1 @ slice2 in 
      (pol', slice')
    | Seq (pol1, pol2) ->
      let pol1',slice1 = desugar_pol curr pol1 in  
      let pol2',slice2 = desugar_pol curr pol2 in  
      let pol' = NCT.Seq (pol1', pol2') in 
      let slice' = slice1 @ slice2 in 
      (pol', slice')
    | ITE (pred, then_pol, else_pol) ->
      let (then_po, slice1) = desugar_pol curr then_pol in
      let (else_po, slice2) = desugar_pol curr else_pol in
      (NCT.ITE (desugar_pred pred, then_po, else_po), 
       slice1 @ slice2)
    | Empty -> 
      let pol' = NCT.Filter NCT.Nothing in 
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
            (fun acc s -> NCT.And(acc, NCT.Hdr(NCT.dlVlan s)))
            NCT.Everything sslice'
        else 
          NCT.Nothing in
      let pred_curr = NCT.Hdr(NCT.dlVlan(curr)) in 
      let pred_next = NCT.Hdr(NCT.dlVlan(next)) in 
      let pol1' = 
        NCT.Union 
          ( NCT.Seq 
            ( NCT.Filter(NCT.And(pred_curr, sin'))
            , NCT.Action 
              (NetCore_Action.Output.updateDlVlan curr next))
          , NCT.Filter (NCT.Or (pred_next, pred_rec))) in 
      let pol2' = spol' in 
      let pol3' = 
        NCT.Union ( NCT.Seq ( NCT.Filter(NCT.And(pred_next, sout'))
                        , NCT.Action (NetCore_Action.Output.updateDlVlan next curr))
                , NCT.Filter(NCT.Not(NCT.And(pred_next, sout')))) in 
      let pol' = NCT.Seq(pol1', NCT.Seq(pol2', pol3')) in 
      let slice' = next::sslice' in 
      (pol', slice') in 
  fst (desugar_pol None pol)
