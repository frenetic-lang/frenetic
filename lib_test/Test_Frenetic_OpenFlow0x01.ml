open Frenetic_OpenFlow0x01
open QuickCheck

module Header = Frenetic_OpenFlow_Header

module RoundTripping = struct
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

  module Gen = Arbitrary_Frenetic_OpenFlow0x01
 
  let%test "OpenFlow_Header RoundTrip" =
    let module GenHeader = Gen.Frenetic_OpenFlow0x01_Unsize(Arbitrary_Frenetic_OpenFlow_Header.Header) in
      (openflow_quickCheck GenHeader.arbitrary
          GenHeader.to_string GenHeader.parse GenHeader.marshal)

  let%test "OpenFlow0x01 Wildcards RoundTrip" =
      (openflow_quickCheck Gen.Wildcards.arbitrary
          Gen.Wildcards.to_string Gen.Wildcards.parse Gen.Wildcards.marshal)

  let%test "OpenFlow0x01 Match RoundTrip" =
      let module GenMatch = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.Match) in
      (openflow_quickCheck GenMatch.arbitrary
          GenMatch.to_string GenMatch.parse GenMatch.marshal)

  let%test "OpenFlow0x01 PseudoPort RoundTrip" =
      (openflow_quickCheck Gen.PseudoPort.arbitrary
          Gen.PseudoPort.to_string Gen.PseudoPort.parse Gen.PseudoPort.marshal)

  let%test "OpenFlow0x01 Action RoundTrip" =
      let module GenAction = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.Action) in
      (openflow_quickCheck GenAction.arbitrary
          GenAction.to_string GenAction.parse GenAction.marshal)

  let%test "OpenFlow0x01 Timeout RoundTrip" =
      let module GenTimeout = Gen.Timeout in
      (openflow_quickCheck GenTimeout.arbitrary
          GenTimeout.to_string GenTimeout.parse GenTimeout.marshal)

  let%test "OpenFlow0x01 FlowMod.Command RoundTrip" =
      let module GenCommand = Gen.FlowMod.Command in
      (openflow_quickCheck GenCommand.arbitrary
          GenCommand.to_string GenCommand.parse GenCommand.marshal)

  let%test "OpenFlow0x01 FlowMod RoundTrip" =
      let module GenFlowMod = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.FlowMod) in
      (openflow_quickCheck GenFlowMod.arbitrary
          GenFlowMod.to_string GenFlowMod.parse GenFlowMod.marshal)

  let%test "OpenFlow0x01 FlowRemoved.Reason RoundTrip" =
      let module GenReason = Gen.FlowRemoved.Reason in
      (openflow_quickCheck GenReason.arbitrary
          GenReason.to_string GenReason.parse GenReason.marshal)

  let%test "OpenFlow0x01 FlowRemoved RoundTrip" =
      let module GenFlowRemoved = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.FlowRemoved) in
      (openflow_quickCheck GenFlowRemoved.arbitrary
          GenFlowRemoved.to_string GenFlowRemoved.parse GenFlowRemoved.marshal)

  let%test "OpenFlow0x01 PortDescription.PortConfig RoundTrip" =
      let module GenPortConfig = Gen.PortDescription.PortConfig in
      (openflow_quickCheck GenPortConfig.arbitrary
          GenPortConfig.to_string GenPortConfig.parse GenPortConfig.marshal)

  let%test "OpenFlow0x01 PortDescription.PortState RoundTrip" =
      let module GenPortState = Gen.PortDescription.PortState in
      (openflow_quickCheck GenPortState.arbitrary
          GenPortState.to_string GenPortState.parse GenPortState.marshal)

  let%test "OpenFlow0x01 PortDescription RoundTrip" =
      let module GenPortDescription = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.PortDescription) in
      (openflow_quickCheck GenPortDescription.arbitrary
          GenPortDescription.to_string GenPortDescription.parse GenPortDescription.marshal)

  let%test "OpenFlow0x01 PortStatus RoundTrip" =
      let module GenPortStatus = Gen.Frenetic_OpenFlow0x01_Unsize(Gen.PortStatus) in
      (openflow_quickCheck GenPortStatus.arbitrary
          GenPortStatus.to_string GenPortStatus.parse GenPortStatus.marshal)

  let%test "OpenFlow Hello Test 1" = 
    let open Message in 
    let bs = Cstruct.create 101 in
    let m = Hello bs in 
    let x = 42l in 
    let s = marshal x m in  
    let h = Header.parse (Cstruct.of_string s) in 
    let s' = String.sub s Header.size (h.Header.length - Header.size) in 
    let x',m' = parse h s' in 
    let xid_ok = x = x' in 
    let msg_ok = 
      match m',m with 
      | Hello bs', Hello bs ->
        Cstruct.to_string bs = Cstruct.to_string bs'
      | _ -> 
        false in 
          xid_ok && msg_ok 
  
  let%test "OpenFlow Vendor Test 1" =
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
  
  let%test "OpenFlow StatsReply DescriptionReply Test 1" =
    let open Message in
    let bs' = Cstruct.create 1060 in
    let content = {  
      manufacturer = Bytes.create 256
      ; hardware = Bytes.create 256
      ; software = Bytes.create 256
      ; serial_number = Bytes.create 32
      ; datapath = Bytes.create 256} in
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
