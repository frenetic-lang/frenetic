open NetCore_Types
open Packet
open Format

let desugar 
  (genvlan : unit -> int option)
  (pol : External.policy) :
  Internal.pol =
  let open Internal in
  let open External in
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
    | GetPacketCount (d, handler) ->
      NetCore_Action.Output.packet_query (d, handler)
    | GetByteCount (d, handler) ->
      NetCore_Action.Output.byte_query (d, handler) in
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
