open Frenetic_Bit

exception UnparsablePacket of string

type bytes = Cstruct.t

type int8 = int

type int16 = int

type int48 = int64

type dlAddr = int48

type dlTyp = int16

type dlVlan = int16 option

type dlVlanPcp = int8

type nwAddr = int32

type nwProto = int8

type nwTos = int8

type tpPort = int16

let get_byte (n:int64) (i:int) : int =
  if i < 0 or i > 5 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i)))

let bytes_of_mac (x:int64) : string =
  let byte n = Char.chr (get_byte x n) in
  Format.sprintf "%c%c%c%c%c%c"
    (byte 5) (byte 4) (byte 3)
    (byte 2) (byte 1) (byte 0)

let mac_of_bytes (str:string) : int64 =
  if String.length str != 6 then
    raise (Invalid_argument
             (Format.sprintf "mac_of_bytes expected six-byte string, got %d
                              bytes" (String.length str)));
  let byte n = Int64.of_int (Char.code (String.get str n)) in
  let open Int64 in
  logor (shift_left (byte 0) (8 * 5))
    (logor (shift_left (byte 1) (8 * 4))
       (logor (shift_left (byte 2) (8 * 3))
          (logor (shift_left (byte 3) (8 * 2))
             (logor (shift_left (byte 4) (8 * 1))
                (byte 5)))))


module Tcp = struct

  module Flags = struct

    type t =
      { ns : bool
      ; cwr : bool
      ; ece : bool
      ; urg : bool
      ; ack : bool
      ; psh : bool
      ; rst : bool
      ; syn : bool
      ; fin : bool }

    let to_string f = Printf.sprintf
      "{ ns = %B; cwr = %B; ece = %B; urg = %B; ack = %B; psh = %B; rst = %B; \
         syn = %B; fin = %B }"
      f.ns f.cwr f.ece f.urg f.ack f.psh f.rst f.syn f.fin

    let to_int f = 
      let ret = Int32.zero in
      let ret = bit ret 0 f.ns in
      let ret = bit ret 1 f.cwr in
      let ret = bit ret 2 f.ece in
      let ret = bit ret 3 f.urg in
      let ret = bit ret 4 f.ack in
      let ret = bit ret 5 f.psh in
      let ret = bit ret 6 f.rst in
      let ret = bit ret 7 f.syn in
      let ret = bit ret 8 f.fin in
      Int32.to_int ret

    let of_int d =
      { ns  = test_bit 0 d
      ; cwr = test_bit 1 d
      ; ece = test_bit 2 d
      ; urg = test_bit 3 d
      ; ack = test_bit 4 d
      ; psh = test_bit 5 d
      ; rst = test_bit 6 d
      ; syn = test_bit 7 d
      ; fin = test_bit 8 d }

  end

  type t = 
    { src : tpPort
    ; dst : tpPort
    ; seq : int32
    ; ack : int32
    ; offset : int8
    ; flags : Flags.t
    ; window : int16
    ; chksum : int8
    ; urgent : int8
    ; payload : bytes }

  cstruct tcp { 
    uint16_t src;
    uint16_t dst;
    uint32_t seq;
    uint32_t ack;
    uint8_t offset; (* offset and reserved *)
    uint8_t flags; 
    uint16_t window;
    uint16_t chksum;
    uint16_t urgent;
    uint32_t options (* options and padding *)
  } as big_endian

  let to_string t = failwith "NYI: Tcp.to_string"

  (** TODO(arjun): errors if size is wrong *)
  let parse (bits : Cstruct.t) = 
    let src = get_tcp_src bits in 
    let dst = get_tcp_dst bits in 
    let seq = get_tcp_seq bits in 
    let ack = get_tcp_ack bits in 
    let offset = get_tcp_offset bits in 
    let offset = offset lsr 4 in 
    let _ = offset land 0x0f in 
    let flags = Flags.of_int (Int32.of_int (get_tcp_flags bits)) in 
    let window = get_tcp_window bits in 
    let chksum = get_tcp_chksum bits in 
    let urgent = get_tcp_urgent bits in 
    let payload = Cstruct.shift bits sizeof_tcp in (* JNF: options fixme *)
    { src = src;
      dst = dst;
      seq = seq;
      ack = ack;
      offset =  offset;
      flags = flags;
      window = window;
      chksum = chksum;
      urgent = urgent;
      payload = payload }

  (* TODO(arjun): should include payload size too *)
  let len (pkt : t) = sizeof_tcp
    
  let serialize (bits : Cstruct.t) (pkt : t) =
    set_tcp_src bits pkt.src;
    set_tcp_dst bits pkt.dst;
    set_tcp_seq bits pkt.seq;
    set_tcp_ack bits pkt.ack;
    set_tcp_offset bits pkt.offset;
    set_tcp_flags bits (Flags.to_int pkt.flags);
    set_tcp_window bits pkt.window;
    set_tcp_window bits pkt.window;
    let bits = Cstruct.shift bits sizeof_tcp in 
    (* TODO(arjun): I think the order is wrong here. It is source first,
       then destination, I think. *)
    Cstruct.blit bits 0 pkt.payload 0 (Cstruct.len pkt.payload)

end

module Icmp = struct

  type t = {
    typ : int8;
    code : int8;
    chksum : int16;
    payload : bytes
  }

  cstruct icmp { 
    uint8_t typ;
    uint8_t code;
    uint16_t chksum
  } as big_endian

  let to_string v = failwith "NYI: Icmp.to_string"

  (* TODO(arjun): error if not enough bytes for header *)
  let parse (bits : Cstruct.t) = 
    let typ = get_icmp_typ bits in
    let code = get_icmp_code bits in
    let chksum = get_icmp_chksum bits in
    let payload = Cstruct.shift bits sizeof_icmp in
    { typ = typ; code = code; chksum = chksum; payload = payload }

  (* TODO(arjun): length of payload too *)
  let len (pkt: t) = sizeof_icmp

  (* TODO(arjun): error if not enough space for packet *)
  let serialize (bits : Cstruct.t) (pkt : t) =
    set_icmp_typ bits pkt.typ;
    set_icmp_code bits pkt.code;
    set_icmp_chksum bits pkt.chksum;
    let bits = Cstruct.shift bits sizeof_icmp in
    (* TODO(arjun): I think the order is wrong here. It is source first,
       then destination, I think. *)
    Cstruct.blit bits 0 pkt.payload 0 (Cstruct.len pkt.payload)
  
end

module Ip = struct

  type tp =
    | Tcp of Tcp.t
    | Icmp of Icmp.t
    | Unparsable of (nwProto * bytes)

  module Flags = struct
  (** [Flags] is the type of IPv4 flags. *)

    type t =
      { df : bool (** Don't fragment. *)
      ; mf : bool (** More fragments. *)
      }

    let to_string v = Printf.sprintf
      "{ df = %B; mf = %B }"
      v.df v.mf

    let of_int d =
      { df = test_bit 1 d
      ; mf = test_bit 2 d }

    let to_int v =
      let ret = Int32.zero in
      let ret = bit ret 1 v.df in
      let ret = bit ret 2 v.mf in
      Int32.to_int ret

  end

  type t = {
    tos : nwTos;
    ident : int16;
    flags : Flags.t;
    frag : int16;
    ttl : int8;
    chksum : int16;
    src : nwAddr;
    dst : nwAddr;
    tp : tp
  }

  cenum ip_proto { 
    IP_ICMP = 0x01;
    IP_TCP = 0x06;
    IP_UDP = 0x11
  } as uint8_t

  cstruct ip { 
    uint8_t vhl; (* version and ihl *)
    uint8_t tos; 
    uint16_t len;
    uint16_t ident;
    uint16_t frag; (* flags and frag *)
    uint8_t ttl;
    uint8_t proto;
    uint16_t chksum;
    uint32_t src;
    uint32_t dst;
    uint32_t options (* options and padding *)
  } as big_endian

  let to_string v = failwith "NYI: Ip.to_string"

  (* TODO(arjun): error if header does not fit *)
  let  parse (bits : Cstruct.t) = 
    let vhl = get_ip_vhl bits in 
    (* TODO(arjun): MUST test for IPv4. Students' machines configured with
       IPv6 will otherwise explode. *)
    let _ = vhl lsr 4 in (* TODO(jnf): test for IPv4? *)
    let ihl = vhl land 0x0f in 
    let tos = get_ip_tos bits in 
    let frag = get_ip_frag bits in 
    let flags = Flags.of_int (Int32.of_int (frag lsr 13)) in
    let frag = frag land 0x1fff in 
    let ttl = get_ip_ttl bits in 
    let ident = get_ip_ident bits in 
    let proto = get_ip_proto bits in 
    let chksum = get_ip_chksum bits in 
    let src = get_ip_src bits in 
    let dst = get_ip_dst bits in 
    let bits = Cstruct.shift bits (ihl * 4) in 
    let tp = match int_to_ip_proto proto with 
      | Some IP_ICMP -> Icmp (Icmp.parse bits)
      | Some IP_TCP -> Tcp (Tcp.parse bits)
      | _ -> Unparsable (proto, bits) in
    { tos = tos;
      ident = ident;
      flags = flags;
      frag = frag;
      ttl = ttl;
      chksum = chksum;
      src = src;
      dst = dst;     
      tp = tp }

  let len (pkt : t) = 
  (* TODO(arjun): what is this hack? *)
    let ip_len = sizeof_ip - 4 in (* JNF: hack! *)
    let tp_len = match pkt.tp with 
      | Tcp tcp -> Tcp.len tcp
      | Icmp icmp -> Icmp.len icmp
      | Unparsable (_, data) -> Cstruct.len data in 
    ip_len + tp_len

  (* TODO(arjun): error if not enough space *)
  let serialize (bits : Cstruct.t) (pkt:t) =
    let v = 4 in (* IP version 4. *)
    let ihl = 5 in (* We don't support IPv4 options at the moment. *)
    let vhl = (v lsl 4) lor ihl in
    set_ip_vhl bits vhl;
    set_ip_tos bits pkt.tos;
    set_ip_ident bits pkt.tos;
    set_ip_frag bits ((Flags.to_int pkt.flags) lor (pkt.frag lsl 13));
    set_ip_ttl bits pkt.ttl;
    let proto = match pkt.tp with
      | Tcp _ -> 6
      | Icmp _ -> 1
      | Unparsable (p, _) -> p in
    set_ip_proto bits proto;
    set_ip_chksum bits pkt.chksum; (* TODO(arjun): calculate checksum *)
    set_ip_src bits pkt.src;
    set_ip_dst bits pkt.dst;
    let bits = Cstruct.shift bits sizeof_ip in 
    match pkt.tp with
      | Tcp tcp -> 
        Tcp.serialize bits tcp
      | Icmp icmp -> 
        Icmp.serialize bits icmp
      | Unparsable (protocol, data) ->
        Cstruct.blit data 0 bits 0 (Cstruct.len data)

end

module Arp = struct

  type t =
    | Query of dlAddr * nwAddr * nwAddr
    | Reply of dlAddr * nwAddr * dlAddr * nwAddr

  (* Network *)
  cstruct arp {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa
  } as big_endian

  let to_string v = failwith "NYI: Arp.to_string"

  let nwSrc t = match t with
    | Query (_, ip, _) -> ip
    | Reply (_, ip, _, _) -> ip

  let nwDst t = match t with
    | Query (_, _, ip) -> ip
    | Reply (_, _, _, ip) -> ip

  cenum arp_oper {
    ARP_REQUEST = 0x0001;
    ARP_REPLY = 0x0002
  } as uint16_t
  
  (* TODO(arjun): error if not enough space *)
  let parse (bits : Cstruct.t) =
    let oper = get_arp_oper bits in 
    let sha = mac_of_bytes (Cstruct.to_string (get_arp_sha bits)) in 
    let spa = (get_arp_spa bits) in 
    let tpa = (get_arp_tpa bits) in 
    match int_to_arp_oper oper with 
      | Some ARP_REQUEST -> 
        Query (sha, spa, tpa)
      | Some ARP_REPLY -> 
        let tha = mac_of_bytes (Cstruct.to_string (get_arp_tha bits)) in 
        Reply(sha, spa, tha, tpa)
      | _ -> 
        raise (UnparsablePacket 
          (Printf.sprintf "unrecognized ARP operation code (%d)" oper))

  (* TODO(arjun): both requests and replies have the same size? *)
  let len pk = sizeof_arp

  (* TODO(arjun): error if not enough space *)
  let serialize (bits : Cstruct.t) (pkt : t) =
    (* NOTE(ARJUN, JNF): ARP packets specify the size of L2 addresses, so 
       they can be used with IPv6. This version assumes we are doing IPv4. *)
    set_arp_htype bits 1;
    set_arp_ptype bits 0x800;
    set_arp_hlen bits 6;
    set_arp_plen bits 4;
    match pkt with 
      | Query (sha, spa, tpa) -> 
        set_arp_oper bits (arp_oper_to_int ARP_REQUEST);
        set_arp_sha (bytes_of_mac sha) 0 bits;
        set_arp_spa bits spa;
        set_arp_tpa bits tpa;
      | Reply (sha, spa, tha, tpa) -> 
        set_arp_oper bits (arp_oper_to_int ARP_REPLY);
        set_arp_sha (bytes_of_mac sha) 0 bits;
        set_arp_spa bits spa;
        set_arp_tha (bytes_of_mac tha) 0 bits;
        set_arp_tpa bits tpa

end

type nw =
  | Ip of Ip.t
  | Arp of Arp.t
  | Unparsable of (dlTyp * bytes)

type packet = {
  dlSrc : dlAddr;
  dlDst : dlAddr; 
  dlVlan : dlVlan;
  dlVlanPcp : dlVlanPcp;
  nw : nw
}

let nwSrc pkt = match pkt.nw with
  | Ip ip -> ip.Ip.src
  | Arp arp -> Arp.nwSrc arp
  | Unparsable _ -> raise (Invalid_argument "nwSrc: unparsable packet")

let nwDst pkt = match pkt.nw with
  | Ip ip -> ip.Ip.dst
  | Arp arp -> Arp.nwDst arp
  | Unparsable _ -> raise (Invalid_argument "nwDst: unparsable packet")

let nwProto pkt = match pkt.nw with 
  | Ip ip -> 
    begin match ip.Ip.tp with
      | Ip.Tcp _ -> 6
      | Ip.Icmp _ -> 1
      | Ip.Unparsable (p, _) -> p
    end
  | Arp _ -> raise (Invalid_argument "nwProto: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "nwProto: unparsable packet")

let nwTos pkt = match pkt.nw with 
  | Ip ip -> ip.Ip.tos
  | Arp _ -> raise (Invalid_argument "nwTos: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "nwTos: unparsable packet")

let tpSrc pkt = match pkt.nw with 
  | Ip ip ->
    (match ip.Ip.tp with
    | Ip.Tcp frg -> frg.Tcp.src
    | Ip.Icmp _ -> raise (Invalid_argument "tpSrc: ICMP packet")
    | Ip.Unparsable _ -> 
      raise (Invalid_argument "tpSrc: cannot parse body of IP packet"))
  | Arp _ -> raise (Invalid_argument "tpSrc: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "tpSrc: unparsable packet")

let tpDst pkt = match pkt.nw with 
  | Ip ip ->
    (match ip.Ip.tp with
    | Ip.Tcp frg -> frg.Tcp.dst
    | Ip.Icmp _ -> raise (Invalid_argument "tpDst: ICMP packet")
    | Ip.Unparsable _ -> 
      raise (Invalid_argument "tpDst: cannot parse body of IP packet"))
  | Arp _ -> raise (Invalid_argument "tpDst: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "tpDst: unparsable packet")

let setDlSrc pkt dlSrc =
  { pkt with dlSrc = dlSrc }

let setDlDst pkt dlDst =
  { pkt with dlDst = dlDst }

let setDlVlan pkt dlVlan =
  { pkt with dlVlan = dlVlan }

let setDlVlanPcp pkt dlVlanPcp =
  { pkt with dlVlanPcp = dlVlanPcp }

let nw_setNwSrc nwPkt src = match nwPkt with
  | Ip ip ->
    Ip { ip with Ip.src = src }
  | nw -> 
    nw

let nw_setNwDst nwPkt dst = match nwPkt with
  | Ip ip ->
    Ip { ip with Ip.dst = dst }
  | nw -> 
    nw

let nw_setNwTos nwPkt tos =
  match nwPkt with
  | Ip ip ->
    Ip { ip with Ip.tos = tos }
  | nw -> 
    nw
    
let setNwSrc pkt nwSrc =
  { pkt with nw = nw_setNwSrc pkt.nw nwSrc }

let setNwDst pkt nwDst = 
  { pkt with nw = nw_setNwDst pkt.nw nwDst }

let setNwTos pkt nwTos =
  { pkt with nw = nw_setNwTos pkt.nw nwTos }

let tp_setTpSrc tp src = match tp with 
  | Ip.Tcp tcp ->
    Ip.Tcp { tcp with Tcp.src = src } (* JNF: checksum? *)
  | tp -> 
    tp

let tp_setTpDst tp dst = match tp with 
  | Ip.Tcp tcp ->
    Ip.Tcp { tcp with Tcp.dst = dst } (* JNF: checksum? *)
  | tp -> 
    tp

let nw_setTpSrc nwPkt tpSrc = match nwPkt with 
  | Ip ip ->
    Ip { ip with Ip.tp = tp_setTpSrc ip.Ip.tp tpSrc }
  | nw -> 
    nw

let nw_setTpDst nwPkt tpDst = match nwPkt with 
  | Ip ip ->
    Ip { ip with Ip.tp = tp_setTpDst ip.Ip.tp tpDst }
  | nw -> 
    nw

let setTpSrc pkt tpSrc =
  { pkt with nw = nw_setTpSrc pkt.nw tpSrc }

let setTpDst pkt tpDst =
  { pkt with nw = nw_setTpDst pkt.nw tpDst }

let string_of_mac (x:int64) : string =
  Format.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
    (get_byte x 5) (get_byte x 4) (get_byte x 3)
    (get_byte x 2) (get_byte x 1) (get_byte x 0)

let get_byte32 (n : Int32.t) (i : int) : int = 
  let open Int32 in
  if i < 0 or i > 3 then
    raise (Invalid_argument "get_byte32 index out of range");
  to_int (logand 0xFFl (shift_right_logical n (8 * i)))

let get_dlTyp nw = match nw with
    | Ip _ -> 0x800
    | Arp _ -> 0x806
    | Unparsable (t, _) -> t

let dlTyp pkt = get_dlTyp pkt.nw

let string_of_ip (ip : Int32.t) : string = 
  Format.sprintf "%d.%d.%d.%d" (get_byte32 ip 3) (get_byte32 ip 2) 
    (get_byte32 ip 1) (get_byte32 ip 0)

let string_of_dlAddr = string_of_mac

let string_of_dlTyp = string_of_int

let string_of_dlVlan = function
  | None -> "none"
  | Some n -> string_of_int n

let string_of_dlVlanPcp = string_of_int

let string_of_nwAddr = Int32.to_string

let string_of_nwProto = string_of_int

let string_of_nwTos = string_of_int

let string_of_tpPort = string_of_int

let string_of_nw nw = "Not yet implemented"

let string_of_packet 
    { dlSrc = pktDlSrc;
      dlDst = pktDlDst;
      dlVlan = pktDlVlan;
      dlVlanPcp = pktDlVlanPcp;
      nw = nw } = 
  let dlTyp = get_dlTyp nw in
  Printf.sprintf "(%s, %s, %s, %s, %s, %s)"
    (string_of_dlAddr pktDlSrc)
    (string_of_dlAddr pktDlDst)
    (string_of_int dlTyp)
    (string_of_dlVlan pktDlVlan)
    (string_of_dlVlanPcp pktDlVlanPcp)
    (string_of_nw nw)

(* Data Link *)
cstruct eth {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t typ
} as big_endian

cstruct vlan {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t hdr; (* 0x8100 *)
  uint16_t tag; (* tag and pcp *)
  uint16_t typ
} as big_endian

cenum eth_typ {
  ETHTYP_IP = 0x0800;
  ETHTYP_ARP = 0x0806;
  ETHTYP_VLAN = 0x8100
} as uint16_t

let vlan_mask = 0xfff
let vlan_pcp_mask = 0x7 lsl 9

(* Transport *)

cstruct udp { 
  uint16_t src;
  uint16_t dst;
  uint16_t len;
  uint16_t chksum
} as big_endian

(* TODO(arjun): error if not enough space *)
let parse (bits : Cstruct.t) =
  let src = Cstruct.to_string (get_eth_src bits) in
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let typ = get_eth_typ bits in 
  let (vlan_tag, vlan_pcp, typ, offset) = 
    match int_to_eth_typ typ with 
      | Some ETHTYP_VLAN -> 
        let tag_and_pcp = get_vlan_tag bits in 
        let vlan_tag = tag_and_pcp land 0xfff in 
        let vlan_pcp = tag_and_pcp lsr 13 in 
        let typ = get_vlan_typ bits in 
        (Some vlan_tag, vlan_pcp, typ, sizeof_vlan)
      | _ -> 
        (None, 0x0, typ, sizeof_eth) in 
  let bits = Cstruct.shift bits offset in
  let nw_header = match int_to_eth_typ typ with 
    | Some ETHTYP_IP -> Ip (Ip.parse bits)
    | Some ETHTYP_ARP -> 
      begin try 
        Arp (Arp.parse bits)
      with UnparsablePacket _ -> 
        Unparsable (typ, Cstruct.of_string (Cstruct.to_string bits))
      end
    | _ -> 
      Unparsable (typ, Cstruct.of_string (Cstruct.to_string bits)) in 
  { dlSrc = mac_of_bytes src;
    dlDst = mac_of_bytes dst;
    dlVlan = vlan_tag;
    dlVlanPcp = vlan_pcp;
    nw = nw_header }

let len (pkt : packet) =
  let eth_len = 
    if pkt.dlVlan != None then sizeof_vlan 
    else sizeof_eth in 
  let nw_len = match pkt.nw with 
    | Ip ip -> Ip.len ip
    | Arp arp -> Arp.len arp 
    | Unparsable (_, data) -> Cstruct.len data in 
  eth_len + nw_len

let serialize_helper (bits : Cstruct.t) (pkt : packet) =
  set_eth_src (bytes_of_mac pkt.dlSrc) 0 bits;
  set_eth_dst (bytes_of_mac pkt.dlDst) 0 bits;
  let dlTyp = get_dlTyp pkt.nw in
  let bits =
    match pkt.dlVlan with
      | Some v ->
        set_vlan_hdr bits 0x8100;
        set_vlan_tag bits ((pkt.dlVlanPcp lsl 13) lor v);
        set_vlan_typ bits dlTyp;
        Cstruct.shift bits sizeof_vlan
      | None ->
        set_eth_typ bits dlTyp;
        Cstruct.shift bits sizeof_eth in
  match pkt.nw with 
    | Ip ip -> Ip.serialize bits ip
    | Arp arp -> Arp.serialize bits arp
    | Unparsable (_, data) -> Cstruct.blit data 0 bits 0 (Cstruct.len data)

let serialize (pkt:packet) : Cstruct.t = 
  let bits = Cstruct.create (len pkt) in 
  let () = serialize_helper bits pkt in 
  bits

(* let parse_udp (bits:Cstruct.t) : udp option =  *)
(*   let src = get_ucp_src bits in  *)
(*   let dst = get_ucp_dst bits in  *)
(*   let chksum = get_udp_chksum bits in  *)
(*   let payload = Cstruct.shift bits sizeof_udp in  *)
(*   Some { udpSrc = src; *)
(*          udpDst = dst; *)
(*          udpChksum = seq; *)
(*          udpPayload = payload } *)

(* Pretty Printing *)
let string_of_nw pkt = 
  match pkt with 
    | Unparsable (typ, data) -> 
      Printf.sprintf "(%d, %s [%d:%d%d%d])"
        typ
        (Cstruct.debug data)  (* TODO(arjun): doesn't work as expected!! *)
        (Cstruct.len data)
        (Char.code (Cstruct.get_char data 0))
        (Char.code (Cstruct.get_char data 1))
        (Char.code (Cstruct.get_char data 2))
    | _ -> ""
      
let string_of_eth pkt = 
  let vlan_tag = 
    match pkt.dlVlan with
    | Some v -> string_of_int v
    | None -> "None" in
  let dlTyp = get_dlTyp pkt.nw in
  Printf.sprintf 
    "{ pktDlSrc = %s; pktDlDst = %s; pktDlTyp = %d; pktDlVlan = %s; \
       pktDlVlanPcp = %d; nw = %s }" 
    (string_of_mac pkt.dlSrc)
    (string_of_mac pkt.dlDst)
    dlTyp
    (vlan_tag)
    (pkt.dlVlanPcp)
    (string_of_nw pkt.nw)
