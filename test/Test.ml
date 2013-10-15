open OpenFlow0x01
open OpenFlow0x01_Stats

open QuickCheck

module Gen = QuickCheck_gen

(* Test that `parse` is the left inverse of `marshal` *)
let prop_roundtrip parse marshal e =
    parse (marshal e) = e

(* Setup a quickCheck for a serlalizable OpenFlow datatype *)
let openflow_quickCheck arbitrary show parse marshal = 
    let test = testable_fun arbitrary show testable_bool in
    match quickCheck test (prop_roundtrip parse marshal) with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

module RoundTripping = struct
  module Gen = OpenFlow0x01_Arbitrary

  TEST "OpenFlow0x01 Wildcards RoundTrip" =
      (openflow_quickCheck Gen.Wildcards.arbitrary
          Gen.Wildcards.to_string Gen.Wildcards.parse Gen.Wildcards.marshal)

  TEST "OpenFlow0x01 Match RoundTrip" =
      let module GenMatch = Gen.OpenFlow0x01_Unsize(Gen.Match) in
      (openflow_quickCheck GenMatch.arbitrary
          GenMatch.to_string GenMatch.parse GenMatch.marshal)

  TEST "OpenFlow0x01 PseudoPort RoundTrip" =
      (openflow_quickCheck Gen.PseudoPort.arbitrary
          Gen.PseudoPort.to_string Gen.PseudoPort.parse Gen.PseudoPort.marshal)

  TEST "OpenFlow0x01 Action RoundTrip" =
      let module GenAction = Gen.OpenFlow0x01_Unsize(Gen.Action) in
      (openflow_quickCheck GenAction.arbitrary
          GenAction.to_string GenAction.parse GenAction.marshal)

  TEST "OpenFlow0x01 FlowMod.Command RoundTrip" =
      let module GenCommand = Gen.FlowMod.Command in
      (openflow_quickCheck GenCommand.arbitrary
          GenCommand.to_string GenCommand.parse GenCommand.marshal)

  TEST "OpenFlow0x01 FlowMod.Timeout RoundTrip" =
      let module GenTimeout = Gen.FlowMod.Timeout in
      (openflow_quickCheck GenTimeout.arbitrary
          GenTimeout.to_string GenTimeout.parse GenTimeout.marshal)

  TEST "OpenFlow0x01 FlowMod RoundTrip" =
      let module GenFlowMod = Gen.OpenFlow0x01_Unsize(Gen.FlowMod) in
      (openflow_quickCheck GenFlowMod.arbitrary
          GenFlowMod.to_string GenFlowMod.parse GenFlowMod.marshal)

  TEST "OpenFlow Hello Test 1" = 
    let open Message in 
    let bs = Cstruct.create 101 in
    let m = Hello bs in 
    let x = 42l in 
    let s = marshal x m in  
    let h = Header.parse s in 
    let s' = String.sub s Header.size (Header.len h - Header.size) in 
    let x',m' = parse h s' in 
    let xid_ok = x = x' in 
    let msg_ok = 
      match m',m with 
	    | Hello bs', Hello bs ->
	      Cstruct.to_string bs = Cstruct.to_string bs'
	    | _ -> 
	      false in 
          xid_ok && msg_ok 
  
  TEST "OpenFlow Vendor Test 1" =
    let open Message in
    let bs = Cstruct.create 101 in
    let bs' = Cstruct.create ((Cstruct.len bs) + 4) in
    let body = "42 is the answer" in
    Cstruct.blit_from_string body 0 bs 0 (String.length body);
    let m = (42l, bs) in
    let _ = Vendor.marshal m bs' in
    let m' = Vendor.parse bs' in    
    match m, m' with
    | (42l, bs), (42l, bs') ->
      Cstruct.to_string bs = Cstruct.to_string bs'
    | _ ->
      false
  
  TEST "OpenFlow StatsReply DescriptionReply Test 1" =
    let open Message in
    let bs' = Cstruct.create 1060 in
    let content = {  
      manufacturer = String.create 256
      ; hardware = String.create 256
      ; software = String.create 256
      ; serial_number = String.create 32
      ; datapath = String.create 256} in
    let m = DescriptionRep content in
    let _ = StatsReply.marshal m bs' in
    let m' = StatsReply.parse bs' in
    match m, m' with
    | DescriptionRep rep, DescriptionRep rep' ->
      rep.manufacturer = rep'.manufacturer &&
      rep.hardware = rep'.hardware &&
      rep.serial_number = rep'.serial_number &&
      rep.datapath = rep'.datapath
    | _ -> 
      false
end
