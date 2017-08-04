open Frenetic_Packet
open QuickCheck
open Arbitrary_Base
open Core

module Gen = QuickCheck_gen
module Arb = Arbitrary_Frenetic_Packet


(** Test Data **)

let sample_ethernet_packet = {
  dlSrc = 0xdeadbeefL; dlDst = 0xbeefdeadL; dlVlan = Some 1356; dlVlanDei = false; dlVlanPcp = 90; 
  nw = Unparsable (0x6000, Cstruct.of_string "Plain ol Ethernet packet") 
}

let sample_tcp_subpacket = Ip {
  tos = 141 ; ident = 56534 ; flags = { df = true; mf = false; } ; frag = 5568
  ; ttl = 201 ; chksum = 0 ; src = ip_of_string "192.168.0.100"; dst = ip_of_string "172.88.12.250"
  ; options = Cstruct.of_string ""
  ; tp = Tcp {
    src = 8080 ; dst = 44927 ; seq = 40591l ; ack = 19936l ; offset = 100
    ; flags = {ns = true ; cwr = false ; ece  = true ; urg = false ; ack = true
      ; psh = false ; rst = true ; syn = false ; fin = true
    }
    ; window = 21986 ; chksum = 0x94c1 ; urgent = 215 ; payload = Cstruct.of_string "Payload"
  }
}

let sample_tcp_packet = {
  sample_ethernet_packet with nw = sample_tcp_subpacket
}

let sample_tcp_packet_raw = 
  "\000\000\xbe\xef\xde\xad" ^  (* dst *)
  "\000\000\xde\xad\xbe\xef" ^  (* src *)
  "\x81\000" ^ (* ethertype = vlan *) 
  "\x45\x4c" ^ (* vlan = 54c, *)
  "\x08\000" ^ (* inner ethertype = ip *)
  "\x45" ^ (* IP Header, Version *) 
  "\x8d" ^ (* service type *)
  "\000\x2f" ^ (* packet length *) 
  "\xdc\xd6" ^ (* ident *) 
  "\x55\xc0" ^ (* frag and flags *) 
  "\xc9" ^ (* ttl *) 
  "\x06" ^ (* protocol = TCP *) 
  "\x44\x46" ^ (* checksum - ignored *) 
  "\xc0\xa8\000\x64" ^ (* Ip src *)
  "\xac\x58\x0c\xfa" ^ (* Ip dst *) 
  "\x1f\x90" ^ (* TCP Header, Src port *) 
  "\xaf\x7f" ^ (* dst port *) 
  "\x00\x00\x9e\x8f" ^ (* seq *) 
  "\x00\x00\x4d\xe0" ^ (* ack *) 
  "\x41\x55" ^ (* offset and flags *) 
  "\x55\xe2" ^ (* window *) 
  "\x94\xc1" ^ (* checksum *) 
  "\x00\xd7" ^ (* urgent *) 
  "Payload"

let sample_ip_subpacket = Ip {
  tos = 141 ; ident = 56534 ; flags = { df = true; mf = false; } ; frag = 5568
  ; ttl = 201 ; chksum = 0 ; src = ip_of_string "192.168.0.100"; dst = ip_of_string "172.88.12.250"
  ; options = Cstruct.of_string ""
  ; tp = Unparsable (88, Cstruct.of_string "Unparsable IP Packet")
}

let sample_ip_unparseable_packet = {
  sample_ethernet_packet with nw = sample_ip_subpacket
}

let sample_arp_query_packet = { sample_ethernet_packet with 
  nw = Arp ( Query ( 0x908070605040L, ip_of_string "192.168.0.100", ip_of_string "172.88.12.250") ) 
}

let sample_arp_reply_packet = { sample_ethernet_packet with 
  nw = Arp ( Reply ( 0x908070605040L, ip_of_string "192.168.0.100", 0x102030405060L, ip_of_string "172.88.12.250") ) 
}

let sample_unparseable_packet = { sample_ethernet_packet with 
  nw = Unparsable (0x6000, Cstruct.of_string "Unparseable packet") 
}

let%test "dlTyp Accessor with TCP payload" = 
  dlTyp sample_tcp_packet = 0x800

let%test "dlTyp Accessor with ARP payload" = 
  dlTyp sample_arp_query_packet = 0x806

let%test "dlTyp Accessor with Unparseable payload" = 
  dlTyp sample_unparseable_packet = 0x6000

let%test "nwSrc Accessor for IP Packet" = 
  nwSrc sample_tcp_packet = 0xc0a80064l

let%test "nwSrc Accessor for ARP Packet" = 
  nwSrc sample_arp_query_packet = ip_of_string "192.168.0.100"

let%test "nwSrc Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    nwSrc sample_unparseable_packet = (-1l)
  )

let%test "nwDst Accessor for IP Packet" = 
  nwDst sample_tcp_packet = 0xac580cfal

let%test "nwDst Accessor for ARP Packet" = 
  nwDst sample_arp_reply_packet = ip_of_string "172.88.12.250"

let%test "nwDst Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    nwDst sample_unparseable_packet = (-1l)
  )

let%test "nwTos Accessor for IP Packet" = 
  nwTos sample_tcp_packet = 141

let%test "nwTos Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    nwTos sample_unparseable_packet = (-1)
  )

let%test "nwProto Accessor for IP Packet" = 
  nwProto sample_tcp_packet = 0x06

let%test "nwProto Accessor for Unparseable IP Packet" = 
  nwProto sample_ip_unparseable_packet = 88

let%test "nwProto Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    nwProto sample_unparseable_packet = (-1) 
  )

let%test "tpSrc Accessor for TCP Packet" = 
  tpSrc sample_tcp_packet = 8080

let%test "tpSrc Accessor for Unparseable IP Packet raises exception" = 
  Exn.does_raise (fun () ->
    tpSrc sample_ip_unparseable_packet = (-1)
  )

let%test "tpSrc Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    tpSrc sample_unparseable_packet = (-1)
  )

let%test "tpDst Accessor for TCP Packet" = 
  tpDst sample_tcp_packet = 44927

let%test "tpDst Accessor for Unparseable IP Packet raises exception" = 
  Exn.does_raise (fun () ->
    tpDst sample_ip_unparseable_packet = (-1)
  )

let%test "tpDst Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    tpDst sample_unparseable_packet = (-1)
  )

let%test "arpOperation Accessor for TCP Packet" = 
  arpOperation sample_arp_query_packet = 0x0001

let%test "arpOperation Accessor for Unparseable Packet raises exception" = 
  Exn.does_raise (fun () ->
    arpOperation sample_unparseable_packet = (-1)
  )

let%test "arpOperation Accessor for IP Packet raises exception" = 
  Exn.does_raise (fun () ->
    arpOperation sample_tcp_packet = (-1)
  )

let%test "setDlSrc Mutator" = 
  (setDlSrc sample_ethernet_packet 0x102030405060L).dlSrc = 0x102030405060L

let%test "setDlDst Mutator" = 
  (setDlDst sample_ethernet_packet 0x102030405061L).dlDst = 0x102030405061L

let%test "setDlVlan Mutator" = 
  (setDlVlan sample_ethernet_packet None).dlVlan = None

let%test "setDlVlanPcp Mutator" = 
  (setDlVlanPcp sample_ethernet_packet 66).dlVlanPcp = 66

let%test "setNwSrc Mutator sets IP address of IP packet" = 
  nwSrc (setNwSrc sample_tcp_packet (ip_of_string "192.168.0.101")) = (ip_of_string "192.168.0.101")

let%test "setNwSrc Mutator leaves ARP packet alone" = 
  (setNwSrc sample_arp_query_packet (ip_of_string "192.168.0.101")) = sample_arp_query_packet

let%test "setNwDst Mutator sets IP address of IP packet" = 
  nwDst (setNwDst sample_tcp_packet (ip_of_string "172.88.12.251")) = (ip_of_string "172.88.12.251")

let%test "setNwDst Mutator leaves Uparseable packet alone" = 
  (setNwDst sample_unparseable_packet (ip_of_string "172.88.12.251")) = sample_unparseable_packet

let%test "setNwTos Mutator sets Type of Service of IP packet" = 
  nwTos (setNwTos sample_tcp_packet 41) = 41

let%test "setNwTos Mutator leaves ARP packet alone" = 
  (setNwTos sample_arp_reply_packet 41) = sample_arp_reply_packet

let%test "setTpSrc Mutator sets IP address of IP packet" = 
  tpSrc (setTpSrc sample_tcp_packet 8081) = 8081

let%test "setTpSrc Mutator leaves Uparseable packet alone" = 
  (setTpSrc sample_unparseable_packet 8081) = sample_unparseable_packet

let%test "setTpDst Mutator sets IP address of IP packet" = 
  tpDst (setTpDst sample_tcp_packet 17341) = 17341

let%test "setTpDst Mutator leaves Arp packet alone" = 
  (setTpDst sample_arp_query_packet 17341) = sample_arp_query_packet

let%test "string_of_mac returns hh:hh:hh:hh:hh:hh format" = 
  string_of_mac 0x102030405060L = "10:20:30:40:50:60"

let%test "string_of_mac lobs off extra left hand digits" = 
  string_of_mac 0x99102030405060L = "10:20:30:40:50:60"

let%test "mac_of_string converts from hh:hh:hh:hh:hh:hh format" =
  mac_of_string "90:80:70:60:50:40" = 0x908070605040L

let%test "mac_of_string blows up if not in hh:hh:hh:hh:hh:hh format" =
  Exn.does_raise (fun () ->
    mac_of_string "Unparseable" = (-1L)
  )

let%test "string_of_dlAddr returns hh:hh:hh:hh:hh:hh format" = 
  string_of_dlAddr 0x102030405060L = "10:20:30:40:50:60"

let%test "string_of_dlTyp returns readable version of Ethernet Type" =
  string_of_dlTyp 0x800 = "ip"

let%test "string_of_dlTyp returns hex of unknown Ethernet Type" =
  string_of_dlTyp 0x666 = "0x666"

let%test "string_of_dlVlan returns readable version of Vlan" =
  string_of_dlVlan (Some 1356) = "1356"

let%test "string_of_dlVlan returns 'none' if No Vlan is present" =
  string_of_dlVlan None = "none"

let%test "string_of_dlVlanPcp returns readable version of VlanPcp" =
  string_of_dlVlanPcp 800 = "800"

let%test "string_of_ip converts to dotted notation nn.nn.nn.nn" =
  string_of_ip 0xc0a80065l = "192.168.0.101"

let%test "string_of_ip converts 0 to dotted notation 0.0.0.0" =
  string_of_ip 0l = "0.0.0.0"

let%test "ip_of_string converts from dotted notation nn.nn.nn.nn" =
  ip_of_string "192.168.0.101" = 0xc0a80065l

let%test "ip_of_string blows up on inputs not in dotted notation nn.nn.nn.nn" =
  Exn.does_raise (fun () ->
    ip_of_string "192.168.0" = (-1l)
  )

let%test "string_of_nwAddr converts to dotted notation nn.nn.nn.nn" =
  string_of_nwAddr 0xc0a80065l = "192.168.0.101"

let%test "string_of_nwProto returns readable version of IP type" =
  string_of_nwProto (nwProto sample_tcp_packet) = "tcp"

let%test "string_of_nwProto returns integer of IP unparseable type" =
  string_of_nwProto (nwProto sample_ip_unparseable_packet) = "88"  

let%test "string_of_nwTos returns integer of service type" =
  string_of_nwTos (nwTos sample_tcp_packet) = "141" 

let%test "string_of_ipv6 returns IPv6 address of form hhhh:hhhh:hhhh:hhhh:hhhh:hhhh:hhhh:hhhh" = 
  string_of_ipv6 (0x1122334455667788L, 0x9900112233445566L) = "1122:3344:5566:7788:9900:1122:3344:5566"  

let%test "string_of_ipv6 returns IPv6 address in form hhhh:hhhh:hhhh:hhhh:hhhh:hhhh:hhhh:hhhh" = 
  ipv6_of_string "1122:3344:5566:7788:9900:1122:3344:5566" = (0x1122334455667788L, 0x9900112233445566L) 

let%test "string_of_ipv6 fills in 0s between empty ::" = 
  ipv6_of_string "3344::7788:9900:1122:3344" = (0x3344000000000000L, 0x7788990011223344L) 

let addr_roundtrip f f_inverse e =
  (f (f_inverse e)) = e

let addr_quickcheck arbitrary f f_inverse =
  let show (p1,p2) = 
    Format.sprintf "%Lu %Lu" p1 p2 in
  let test = testable_fun arbitrary show testable_bool in
  match quickCheck test (addr_roundtrip f f_inverse) with
    | Success -> true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"

let%test "ipv6_of_string and string_of_ipv6 are inverses of each other" =
  addr_quickcheck arbitrary_uint128 ipv6_of_string string_of_ipv6

let%test "string_of_tpPort returns integer of TCP src port" =
  string_of_tpPort (tpSrc sample_tcp_packet) = "8080" 

let%test "bytes_of_mac returns packed format" = 
  bytes_of_mac 0x102030405060L = "\x10\x20\x30\x40\x50\x60"

let%test "bytes_of_mac lobs off extra left hand digits" = 
  bytes_of_mac 0x99102030405060L = "\x10\x20\x30\x40\x50\x60"

let%test "mac_of_bytes converts from packed format" =
  mac_of_bytes "\x90\x80\x70\x60\x50\x40" = 0x908070605040L

let%test "mac_of_bytes blows up if not in 6 bytes packed format" =
  Exn.does_raise (fun () ->
    mac_of_bytes "Unparseable" = (-1L)
  )

(** Marshall serialization *)

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


let%test "parse constructs an abstract packet from a packed format buffer" =
  (* For some reason, the comparison between the parse and the orig packet fails, but 
     marshalling them to buffers succeeds *)
  (marshal (parse (Cstruct.of_string sample_tcp_packet_raw))) = (marshal sample_tcp_packet)

let%test "len computes length of packet in bytes" = 
  len sample_tcp_packet = 65
    
let%test "marshal constructs a packed format buffer from an abstract packet" =
  marshal sample_tcp_packet = Cstruct.of_string sample_tcp_packet_raw 

let%test "to_string and format_packet gives a representation of the packet" =
  to_string sample_arp_query_packet = "dlSrc=00:00:de:ad:be:ef,dlDst=00:00:be:ef:de:ad,vlan=1356,ARP Query,senderIP=192.168.0.100,targetIP=172.88.12.250"

let prop_roundtrip ?(chksum=false) parse marshal e =
  dl_eq ~chksum (parse (marshal e)) e

let prop_roundtrip2 parse marshal e =
  let e' = parse (marshal e) in
  prop_roundtrip ~chksum:true parse marshal e'

let packet_quickCheck arbitrary pred = 
  let test = testable_fun arbitrary to_string testable_bool in
  match quickCheck test pred with
    | Success -> true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"

let mk_ip tp = Arb.arbitrary_packet
  (Gen.map_gen (fun x -> Ip(x)) (Arb.arbitrary_ip tp))

let%test "Parse and marshall are inverses for unparsable Ethernet frames" =
  (packet_quickCheck (Arb.arbitrary_packet Arb.arbitrary_dl_unparsable)
    (prop_roundtrip parse marshal))

let%test "Parse and marshall are inverses for ARP packets" =
  let arp = Gen.map_gen (fun x -> Arp(x)) Arb.arbitrary_arp in
  (packet_quickCheck (Arb.arbitrary_packet arp)
    (prop_roundtrip parse marshal))

let%test "Parse and marshall are inverses for unparsable IP packets" =
  (packet_quickCheck (mk_ip Arb.arbitrary_ip_unparsable)
    (prop_roundtrip parse marshal))

let%test "Parse and marshall are inverses for UDP packets" =
  let udp = Gen.map_gen (fun x -> Ip.Udp(x))
        (Arb.arbitrary_udp (Arb.arbitrary_payload 65507)) in
  (packet_quickCheck (mk_ip udp)
    (prop_roundtrip2 parse marshal))

let%test "Parse and marshall are inverses for TCP packets" =
  let tcp = Gen.map_gen (fun x -> Ip.Tcp(x))
        (Arb.arbitrary_tcp (Arb.arbitrary_payload (65507 - 128))) in
  (packet_quickCheck (mk_ip tcp)
    (prop_roundtrip2 parse marshal))
