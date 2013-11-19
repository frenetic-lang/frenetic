open Packet
open QuickCheck

(* Setup a quickCheck for a serlalizable OpenFlow datatype *)
let packet_quickCheck arbitrary pred = 
    let test = testable_fun arbitrary to_string testable_bool in
    match quickCheck test pred with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

module RoundTrip = struct
  module Arb = Packet_Arbitrary

  let packet_eq e1 e2 =
    e1.dlSrc = e2.dlSrc &&
    e1.dlDst = e2.dlDst &&
    e1.dlVlan = e2.dlVlan &&
    e1.dlVlanPcp = e2.dlVlanPcp &&
    match e1.nw, e2.nw with
      | Unparsable (l1, b1), Unparsable (l2, b2) ->
        l1 = l2 && compare (Cstruct.to_string b1) (Cstruct.to_string b2) = 0
      | _, _ ->
        e1 = e2

  let prop_roundtrip parse marshal e =
    packet_eq (parse (marshal e)) e

  TEST "Roundtrip property for unparsable Ethernet frames" =
    (packet_quickCheck (Arb.arbitrary_packet Arb.arbitrary_unparsable)
      (prop_roundtrip parse marshal))

  TEST "Roundtrip property for ARP packets" =
    (packet_quickCheck (Arb.arbitrary_packet Arb.arbitrary_arp)
      (prop_roundtrip parse marshal))
end
