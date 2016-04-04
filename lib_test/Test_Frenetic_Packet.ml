open Frenetic_Packet
open QuickCheck
open Arbitrary_Base

module Gen = QuickCheck_gen

(* Setup a quickCheck for a serlalizable OpenFlow datatype *)
let packet_quickCheck arbitrary pred = 
    let test = testable_fun arbitrary to_string testable_bool in
    match quickCheck test pred with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

module RoundTrip = struct
  module Arb = Arbitrary_Frenetic_Packet

  let unparsable_eq (l1, b1) (l2, b2) =
    l1 = l2 && compare (Cstruct.to_string b1) (Cstruct.to_string b2) = 0

  let udp_eq ?(chksum=false) e1 e2 =
    let open Udp in
    e1.src = e2.src &&
    e1.dst = e2.dst &&
    ((not chksum) || e1.chksum = e2.chksum) &&
    compare (Cstruct.to_string e1.payload) (Cstruct.to_string e2.payload) = 0

  let tcp_eq ?(chksum=false) e1 e2 =
    let open Tcp in
    e1.src = e2.src &&
    e1.dst = e2.dst &&
    e1.seq = e2.seq &&
    e1.ack = e2.ack &&
    e1.offset = e2.offset &&
    e1.flags = e2.flags &&
    e1.window = e2.window &&
    e1.urgent = e2.urgent &&
    ((not chksum) || e1.chksum = e2.chksum) &&
    compare (Cstruct.to_string e1.payload) (Cstruct.to_string e2.payload) = 0

  let ip_eq ?(chksum=false) e1 e2 =
    let open Ip in
    e1.tos = e2.tos &&
    e1.ident = e2.ident &&
    e1.flags = e2.flags &&
    e1.frag = e2.frag &&
    e1.ttl = e2.ttl &&
    e1.src = e2.src &&
    e1.dst = e2.dst &&
    match e1.tp, e2.tp with
      | Unparsable u1, Unparsable u2 ->
        unparsable_eq u1 u2
      | Udp p1, Udp p2 ->
        udp_eq ~chksum p1 p2
      | Tcp p1, Tcp p2 ->
        tcp_eq ~chksum p1 p2
      | _, _ ->
        e1 = e2

  let dl_eq ?(chksum=false) e1 e2 =
    e1.dlSrc = e2.dlSrc &&
    e1.dlDst = e2.dlDst &&
    e1.dlVlan = e2.dlVlan &&
    e1.dlVlanPcp = e2.dlVlanPcp &&
    match e1.nw, e2.nw with
      | Unparsable u1, Unparsable u2 ->
        unparsable_eq u1 u2
      | Ip nw1, Ip nw2 ->
        ip_eq ~chksum nw1 nw2
      | _, _ ->
        e1 = e2

  let prop_roundtrip ?(chksum=false) parse marshal e =
    dl_eq ~chksum (parse (marshal e)) e

  let prop_roundtrip2 parse marshal e =
    let e' = parse (marshal e) in
    prop_roundtrip ~chksum:true parse marshal e'

  let%test "Roundtrip property for unparsable Ethernet frames" =
    (packet_quickCheck (Arb.arbitrary_packet Arb.arbitrary_dl_unparsable)
      (prop_roundtrip parse marshal))

  let%test "Roundtrip property for ARP packets" =
    let arp = Gen.map_gen (fun x -> Arp(x)) Arb.arbitrary_arp in
    (packet_quickCheck (Arb.arbitrary_packet arp)
      (prop_roundtrip parse marshal))

  let mk_ip tp = Arb.arbitrary_packet
    (Gen.map_gen (fun x -> Ip(x)) (Arb.arbitrary_ip tp))

  let%test "Roundtrip property for unparsable IP packets" =
    (packet_quickCheck (mk_ip Arb.arbitrary_ip_unparsable)
      (prop_roundtrip parse marshal))

  let%test "Roundtrip property for UDP packets" =
    let udp = Gen.map_gen (fun x -> Ip.Udp(x))
          (Arb.arbitrary_udp (Arb.arbitrary_payload 65507)) in
    (packet_quickCheck (mk_ip udp)
      (prop_roundtrip2 parse marshal))

  let%test "Roundtrip property for TCP packets" =
    let tcp = Gen.map_gen (fun x -> Ip.Tcp(x))
          (Arb.arbitrary_tcp (Arb.arbitrary_payload (65507 - 128))) in
    (packet_quickCheck (mk_ip tcp)
      (prop_roundtrip2 parse marshal))

  let addr_roundtrip parse marshal e =
    (parse (marshal e)) = e

  let addr_quickcheck arbitrary parse marshal =
    let show (p1,p2) = 
      Format.sprintf "%Lu %Lu" p1 p2 in
    let test = testable_fun arbitrary show testable_bool in
    match quickCheck test (addr_roundtrip parse marshal) with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let%test "Roundtrip property for IPv6 Address" =
    (addr_quickcheck arbitrary_uint128
      ipv6_of_string string_of_ipv6)


end

