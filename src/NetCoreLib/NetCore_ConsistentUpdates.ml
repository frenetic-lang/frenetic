open NetCore_Types

(* No topo info ATM, so overly heavy instrumentation here *)

let allPtrn = 
  { ptrnDlSrc = WildcardAll;
    ptrnDlDst = WildcardAll;
    ptrnDlType = WildcardAll;
    ptrnDlVlan = WildcardAll;
    ptrnDlVlanPcp = WildcardAll;
    ptrnNwSrc = WildcardAll;
    ptrnNwDst = WildcardAll;
    ptrnNwProto = WildcardAll;
    ptrnNwTos = WildcardAll;
    ptrnTpSrc = WildcardAll;
    ptrnTpDst = WildcardAll;
    ptrnInPort = WildcardAll
  }

let internal_policy p ver =
  PoSeq(PoFilter (PrHdr {allPtrn with ptrnDlVlan = WildcardExact (Some ver)}), p)

let edge_policy p ver =
  PoSeq(PoFilter(PrHdr {allPtrn with ptrnDlVlan = WildcardExact None}),
	PoSeq(p, PoAction[SwitchAction {id with outPort = Here; 
	  outDlVlan = Some (None, Some ver)}]))

let prOr = List.fold_left (fun pr1 pr2 -> PrOr(pr1, pr2)) PrNone

let ingressPort pt = PrHdr {allPtrn with ptrnInPort = pt}

let strip_policy ver switches extPorts =
  PoITE(prOr (List.map (fun sw -> (PrAnd(prOr (List.map (fun p -> ingressPort (WildcardExact (Physical p))) (extPorts sw)), PrOnSwitch sw))) switches), 
	PoAction [SwitchAction {id with outPort = Here; outDlVlan = Some (Some ver, None)}], 
	PoAction [SwitchAction {id with outPort = Here}])
  
let gen_update_pols orig ver switches extPorts =
  (PoSeq (internal_policy orig ver, strip_policy ver switches extPorts),
   PoSeq (edge_policy orig ver, strip_policy ver switches extPorts))
