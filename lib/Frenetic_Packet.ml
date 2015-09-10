open Sexplib
open Sexplib.Std
open Frenetic_Bits

(* Exception raised when manipulating malformed packets *) 
exception UnparsablePacket of string

(* Ethernet frame types *)
let ip_code = 0x800
let arp_code = 0x806

(* IP protocol numbers *)
let icmp_code = 0x01
let igmp_code = 0x02
let tcp_code = 0x06
let udp_code = 0x11

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

let string_of_ip (ip : Int32.t) : string = 
  Format.sprintf "%d.%d.%d.%d" (get_byte32 ip 3) (get_byte32 ip 2) 
    (get_byte32 ip 1) (get_byte32 ip 0)

let string_of_ipv6 ((ip1,ip2) : int64*int64) : string =
  Format.sprintf "%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x"
   (get_byte ip1 7) (get_byte ip1 6) (get_byte ip1 5) (get_byte ip1 4)
   (get_byte ip1 3) (get_byte ip1 2) (get_byte ip1 1) (get_byte ip1 0)
   (get_byte ip2 7) (get_byte ip2 6) (get_byte ip2 5) (get_byte ip2 4)
   (get_byte ip2 3) (get_byte ip2 2) (get_byte ip2 1) (get_byte ip2 0)

let string_of_mac (x:int64) : string =
  Format.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
    (get_byte x 5) (get_byte x 4) (get_byte x 3)
    (get_byte x 2) (get_byte x 1) (get_byte x 0)

let bytes_of_sexp s = 
  match s with 
  | Sexp.Atom w -> 
    begin 
      let n = String.length w in 
      let buf = Cstruct.create n in 
      for i = 0 to n - 1 do 
  Cstruct.set_char buf i w.[i]
      done;
      buf
    end
  | _ -> 
    failwith "bytes_of_sexp: expected Atom"

let sexp_of_bytes s = 
  let n = Cstruct.len s in 
  let buf = Buffer.create n in 
  for i = 0 to n - 1 do 
    Buffer.add_char buf (Cstruct.get_char s i)
  done;
  Sexp.Atom (Buffer.contents buf)

type bytes = Cstruct.t

type int8 = int with sexp

type int16 = int with sexp

type int48 = int64 with sexp

type dlAddr = int48 with sexp

type dlTyp = int16 with sexp

type dlVlan = int16 option with sexp

type dlVlanPcp = int8 with sexp

type dlVlanDei = bool with sexp

type nwAddr = int32 with sexp

type nwProto = int8 with sexp

type nwTos = int8 with sexp

type ipv6Addr = int64*int64 with sexp

type tpPort = int16 with sexp

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
      ; fin : bool } with sexp

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
    ; payload : Cstruct.t } with sexp

  let format fmt v =
    let open Format in
    fprintf fmt "@[tpSrc=%d;tpDst=%d@]" v.src v.dst

  cstruct tcp { 
    uint16_t src;
    uint16_t dst;
    uint32_t seq;
    uint32_t ack;
    uint16_t offset_flags; (* offset, reserved, and flags *)
    uint16_t window;
    uint16_t chksum;
    uint16_t urgent
  } as big_endian

  let parse (bits : Cstruct.t) = 
    if Cstruct.len bits < sizeof_tcp then
      raise (UnparsablePacket "not enough bytes for TCP header");
    let src = get_tcp_src bits in 
    let dst = get_tcp_dst bits in 
    let seq = get_tcp_seq bits in 
    let ack = get_tcp_ack bits in 
    let offset = get_tcp_offset_flags bits in
    let offset = offset lsr 12 in
    let _ = offset land 0x0f in 
    let flags = Flags.of_int (Int32.of_int (get_tcp_offset_flags bits)) in
    let window = get_tcp_window bits in 
    let chksum = get_tcp_chksum bits in 
    let urgent = get_tcp_urgent bits in 
    (* TODO(JNF): support options *)
    let payload = Cstruct.shift bits sizeof_tcp in
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

  let len (pkt : t) = sizeof_tcp + Cstruct.len pkt.payload

  (* Assumes that bits has enough room *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    set_tcp_src bits pkt.src;
    set_tcp_dst bits pkt.dst;
    set_tcp_seq bits pkt.seq;
    set_tcp_ack bits pkt.ack;
    let offset_flags = (pkt.offset lsl 12) lor (Flags.to_int pkt.flags) in
    set_tcp_offset_flags bits offset_flags;
    set_tcp_window bits pkt.window;
    set_tcp_chksum bits pkt.chksum;
    set_tcp_urgent bits pkt.urgent;
    let bits = Cstruct.shift bits sizeof_tcp in 
    Cstruct.blit pkt.payload 0 bits 0 (Cstruct.len pkt.payload)

  let checksum (bits : Cstruct.t) (src : nwAddr) (dst : nwAddr) (pkt : t) =
    let length = len pkt in
    let pseudo_header = Cstruct.create 12 in
    let () = 
      Cstruct.BE.set_uint32 pseudo_header 0  src;
      Cstruct.BE.set_uint32 pseudo_header 4  dst;
      Cstruct.set_uint8     pseudo_header 8  0;
      Cstruct.set_uint8     pseudo_header 9  0x6;
      Cstruct.BE.set_uint16 pseudo_header 10 length in 
    set_tcp_chksum bits 0;
    let chksum = Tcpip_checksum.ones_complement_list
      (if (length mod 2) = 0
        then [pseudo_header; Cstruct.sub bits 0 length]
        else [pseudo_header; Cstruct.sub bits 0 length; Cstruct.of_string "\x00"]) in
    set_tcp_chksum bits chksum
end

module Udp = struct

  type t =
    { src : tpPort
    ; dst : tpPort
    ; chksum : int16
    ; payload : Cstruct.t }
  with sexp

  let format fmt v =
    let open Format in
    fprintf fmt "@[tpSrc=%d;tpDst=%d@]" v.src v.dst

  cstruct udp {
    uint16_t src;
    uint16_t dst;
    uint16_t len;
    uint16_t chksum
  } as big_endian

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_udp then
      raise (UnparsablePacket "not enough bytes for UDP header");
    let src = get_udp_src bits in
    let dst = get_udp_dst bits in
    let chksum = get_udp_chksum bits in
    let payload = Cstruct.shift bits sizeof_udp in
    { src = src;
      dst = dst;
      chksum = chksum;
      payload = payload }

  let len (pkt : t) = sizeof_udp + Cstruct.len pkt.payload

  (* Assumes that bits has enough room *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    set_udp_src bits pkt.src;
    set_udp_dst bits pkt.dst;
    set_udp_len bits (sizeof_udp + (Cstruct.len pkt.payload));
    set_udp_chksum bits 0; (* UDP checksum is optional in IPv4 *)
    let bits = Cstruct.shift bits sizeof_udp in
    Cstruct.blit pkt.payload 0 bits 0 (Cstruct.len pkt.payload)

end

module Icmp = struct

  type t = {
    typ : int8;
    code : int8;
    chksum : int16;
    payload : Cstruct.t
  } with sexp

  cstruct icmp { 
    uint8_t typ;
    uint8_t code;
    uint16_t chksum
  } as big_endian

  let format fmt v =
    let open Format in
    match v.typ with
      | 0 -> fprintf fmt "ICMP echo reply";
      | 8 -> fprintf fmt "ICMP echo request"
      | n -> fprintf fmt "ICMP type=%d,code=%d" n v.code

  let parse (bits : Cstruct.t) = 
    if Cstruct.len bits < sizeof_icmp then
      raise (UnparsablePacket "not enough bytes for ICMP header");
    let typ = get_icmp_typ bits in
    let code = get_icmp_code bits in
    let chksum = get_icmp_chksum bits in
    let payload = Cstruct.shift bits sizeof_icmp in
    { typ = typ; code = code; chksum = chksum; payload = payload }

  let len (pkt: t) = sizeof_icmp + Cstruct.len pkt.payload

  (* Assumes that bits has enough room. *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    set_icmp_typ bits pkt.typ;
    set_icmp_code bits pkt.code;
    set_icmp_chksum bits pkt.chksum;
    let bits = Cstruct.shift bits sizeof_icmp in
    Cstruct.blit pkt.payload 0 bits 0 (Cstruct.len pkt.payload)
  
end

let rec indicies_maker n = if n = 0 then [] else [n]@(indicies_maker (n-1));;

(* TODO - enhance type & parsing with individual flags, like TCP or IP *)
(* TODO - add & expose some helpful constants such as A, MX, AAAA, etc. *)
(* TODO - DNS oddities: UTF-8 support, Punycode, & DNS string compression *)
(* TODO - create type hierarchy for RData: A, AAAA, NS, PTR, CNAME, MX, etc. *)

module Dns = struct

  (* Helper to get the RLE-encoded, NULL-terminated names in DNS records *)
  let get_dns_name bits =
    let get_piece = (fun bits ->
                      let len = Cstruct.get_uint8 bits 0 in
                      Cstruct.copy bits 1 len) in
    let rec get_pieces = (fun bits acc ->
                            let piece = get_piece bits in
                            let acc = acc @ [piece] in
                            let len = String.length piece in
                            let bits = Cstruct.shift bits (len + 1) in
                            if len > 0 then get_pieces bits acc
                            else acc) in
    String.concat "." (get_pieces bits []);;

  (* DNS string encoding requires 1 byte per '.' separated piece; since the
     '.' is not included, we gain 1 for the first piece, plus NULL term *)
  let dns_name_len (name : string) =  (String.length name) + 2

  (* Helper to set a DNS name; does not use the (optional) compression scheme.
     Return 'bits' located at next write location. *)
  let set_dns_name (bits : Cstruct.t) (name : string) =
    let pieces = Str.split (Str.regexp "\\.") name in
    let helper = (fun acc piece ->
                  let len = String.length piece in
                  Cstruct.set_uint8 bits acc len;
                  Cstruct.blit_from_string piece 0 bits (acc + 1) len;
                  (acc + len + 1)) in
    let end_pos = List.fold_left helper 0 pieces in
    Cstruct.set_uint8 bits end_pos 0; (* NULL terminator *)
    Cstruct.shift bits (end_pos + 1)


  (* DNS Question Description Records *)
  module Qd = struct

    type t = {
      name : string;
      typ : int16;
      class_ : int16
    } with sexp

    cstruct qd {
      (* preceeded by name *)
      uint16_t typ;
      uint16_t class_
    } as big_endian

    let format fmt v =
      let open Format in
      fprintf fmt "@[;(name=%s;typ=0x%x;class=0x%x)@]"
          v.name v.typ v.class_

    let parse (bits : Cstruct.t) =
      let name = get_dns_name bits in
      let bits = Cstruct.shift bits (dns_name_len name) in
      if Cstruct.len bits < sizeof_qd then
        raise (UnparsablePacket "not enough bytes for QD record");
      let typ = get_qd_typ bits in
      let class_ = get_qd_class_ bits in
      { name = name; typ = typ; class_ = class_ }

    let len (qd : t) = (dns_name_len qd.name) + sizeof_qd

    let marshal (bits : Cstruct.t) (qd : t) =
      let bits = set_dns_name bits qd.name in
      set_qd_typ bits qd.typ;
      set_qd_class_ bits qd.class_;
      Cstruct.shift bits sizeof_qd

  end

  (* DNS Resource Records *)
  module Rr = struct

    type t = {
      name : string;
      typ : int16;
      class_ : int16;
      ttl : int; (* TTL is signed 32-bit int *)
      rdata : Cstruct.t
    } with sexp

    cstruct rr {
      (* preceeded by name *)
      uint16_t typ;
      uint16_t class_;
      int32_t ttl;
      uint16_t rdlen
      (* followed by variable-length RData *)
    } as big_endian

    let format fmt v =
      let open Format in
      fprintf fmt "@[;(name=%s;typ=0x%x;class=0x%x;ttl=%d)@]"
          v.name v.typ v.class_ v.ttl

    let parse (bits : Cstruct.t) =
      let name = get_dns_name bits in
      let bits = Cstruct.shift bits (dns_name_len name) in
      if Cstruct.len bits < sizeof_rr then
        raise (UnparsablePacket "not enough bytes for RR record");
      let typ = get_rr_typ bits in
      let class_ = get_rr_class_ bits in
      let ttl = Int32.to_int (get_rr_ttl bits) in
      let rdlen = get_rr_rdlen bits in
      let rdata = Cstruct.sub bits sizeof_rr rdlen in
      { name = name; typ = typ; class_ = class_;
        ttl = ttl; rdata = rdata }

    let len (rr : t) =
       (dns_name_len rr.name) + sizeof_rr + (Cstruct.len rr.rdata)

    let marshal (bits : Cstruct.t) (rr : t) =
      let bits = set_dns_name bits rr.name in
      set_rr_typ bits rr.typ;
      set_rr_class_ bits rr.class_;
      set_rr_ttl bits (Int32.of_int rr.ttl);
      let rdlen = Cstruct.len rr.rdata in
      set_rr_rdlen bits rdlen;
      Cstruct.blit rr.rdata 0 bits sizeof_rr rdlen;
      Cstruct.shift bits (sizeof_rr + rdlen)

  end


  (* DNS Packet *)
  type t =
    { id : int16
    ; flags : int16
    ; questions : Qd.t list
    ; answers : Rr.t list
    ; authority : Rr.t list
    ; additional : Rr.t list }
  with sexp

  let format fmt v =
    let open Format in
    fprintf fmt "@[id=%x;flags=%x@]" v.id v.flags;
    List.iter (Qd.format fmt) v.questions;
    List.iter (Rr.format fmt) v.answers;
    List.iter (Rr.format fmt) v.authority;
    List.iter (Rr.format fmt) v.additional


  cstruct dns {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount
    (* followed by questions (if any) *)
    (* followed by resource records (if any) *)
  } as big_endian

  let parse_helper (bits : Cstruct.t) (num : int) pf lf off =
    let indices = indicies_maker num in
    let offset = ref (sizeof_dns + off) in
    let get_x = (fun i -> let bits = Cstruct.shift bits (!offset) in
                          let x = pf bits in
                          offset := (!offset + lf x);
                          x) in
    (List.map get_x indices, !offset)

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_dns then
      raise (UnparsablePacket "not enough bytes for DNS header");
    let id = get_dns_id bits in
    let flags = get_dns_flags bits in
    let (qd, off) = parse_helper bits (get_dns_qdcount bits) Qd.parse Qd.len 0 in
    let (an, off) = parse_helper bits (get_dns_ancount bits) Rr.parse Rr.len off in
    let (ns, off) = parse_helper bits (get_dns_nscount bits) Rr.parse Rr.len off in
    let (ar, off) = parse_helper bits (get_dns_arcount bits) Rr.parse Rr.len off in

    { id = id; flags = flags; questions = qd;
      answers = an; authority = ns; additional = ar }

  let len (pkt : t) =
    let tally = fun lfun lst ->
                    List.fold_left (fun acc x -> acc + (lfun x)) 0 lst in
    let qd_len = tally Qd.len pkt.questions in
    let an_len = tally Rr.len pkt.answers in
    let ns_len = tally Rr.len pkt.authority in
    let ar_len = tally Rr.len pkt.additional in
    sizeof_dns + qd_len + an_len + ns_len + ar_len

  (* Assumes that bits has enough room *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    set_dns_id bits pkt.id;
    set_dns_flags bits pkt.flags;
    set_dns_qdcount bits (List.length pkt.questions);
    set_dns_ancount bits (List.length pkt.answers);
    set_dns_nscount bits (List.length pkt.authority);
    set_dns_arcount bits (List.length pkt.additional);
    let bits = Cstruct.shift bits sizeof_dns in
    let bits = List.fold_left Qd.marshal bits pkt.questions in
    let bits = List.fold_left Rr.marshal bits pkt.answers in
    let bits = List.fold_left Rr.marshal bits pkt.authority in
    ignore (List.fold_left Rr.marshal bits pkt.additional)

  let serialize (dns : t) =
    let bits = Cstruct.create (len dns) in
    let () = marshal bits dns in
    bits

  end

module Igmp1and2 = struct

  type t = {
    mrt: int8;
    chksum : int16;
    addr : nwAddr;
  } with sexp

  cstruct igmp1and2 {
    uint8_t mrt;
    uint16_t chksum;
    uint32_t addr
  } as big_endian

  let format fmt v =
    let open Format in
    fprintf fmt "@[mrt=%x;addr=%s@]" v.mrt (string_of_ip v.addr)

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_igmp1and2 then
      raise (UnparsablePacket "not enough bytes for IGMPv1/2 header");
    let mrt = get_igmp1and2_mrt bits in
    let chksum = get_igmp1and2_chksum bits in
    let addr = get_igmp1and2_addr bits in
    { mrt = mrt; chksum = chksum; addr = addr; }

  let len (msg: t) = sizeof_igmp1and2

  (* Assumes that bits has enough room. *)
  let marshal (bits : Cstruct.t) (msg : t) =
    set_igmp1and2_mrt bits msg.mrt;
    set_igmp1and2_chksum bits 0;
    set_igmp1and2_addr bits msg.addr;
    (* ADF: hack since Igmp.sizeof_igmp not defined at this point *)
    let igmp_hdr = Cstruct.sub bits (-1) (1 + sizeof_igmp1and2) in
    let chksum = Tcpip_checksum.ones_complement igmp_hdr in
    set_igmp1and2_chksum bits chksum;


end

module Igmp3 = struct

  (* IGMP v3 Group Records *)
  module GroupRec = struct

    type t = {
      typ : int8;
      addr : nwAddr;
      sources : nwAddr list;
    } with sexp

    cstruct grouprec {
      uint8_t typ;
      uint8_t aux_len;
      uint16_t num_sources;
      uint32_t addr
      (* followed by sources (if any) *)
    } as big_endian

    let format fmt v =
      let open Format in
      fprintf fmt "@[;(typ=%x;addr=%s;sources=%s)@]"
        v.typ
        (string_of_ip v.addr)
        (String.concat "," (List.map string_of_ip v.sources))

    let parse (bits : Cstruct.t) =
      if Cstruct.len bits < sizeof_grouprec then
        raise (UnparsablePacket "not enough bytes for IGMPv3 group record");
      let typ = get_grouprec_typ bits in
      let num_sources = get_grouprec_num_sources bits in
      let addr = get_grouprec_addr bits in
      let indices = indicies_maker num_sources in
      let get_source = fun i -> Cstruct.BE.get_uint32 bits (sizeof_grouprec + ((i-1) * 4)) in
      let sources = List.map get_source indices in
      { typ = typ; addr = addr; sources = sources }

    let len (gr : t) = sizeof_grouprec + (4 * List.length gr.sources)

    let marshal (bits : Cstruct.t) (gr : t) =
      set_grouprec_typ bits gr.typ;
      set_grouprec_aux_len bits 0;
      set_grouprec_num_sources bits (List.length gr.sources);
      set_grouprec_addr bits gr.addr;
      let bits = Cstruct.shift bits sizeof_grouprec in
      List.iteri (fun i v -> Cstruct.BE.set_uint32 bits (i * 4) v) gr.sources;
      Cstruct.shift bits (4 * List.length gr.sources)

  end

  type t = {
    chksum : int16;
    grs : GroupRec.t list;
  } with sexp

  cstruct igmp3 {
    uint8_t reserved1;
    uint16_t chksum;
    uint16_t reserved2;
    uint16_t num_records
    (* followed by group records (if any) *)
  } as big_endian

  let format fmt v =
    let open Format in
    fprintf fmt "@[num_records=%d@]"
        (List.length v.grs);
    List.iter (GroupRec.format fmt) v.grs

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_igmp3 then
      raise (UnparsablePacket "not enough bytes for IGMPv3 header");
    let chksum = get_igmp3_chksum bits in
    let num_records = get_igmp3_num_records bits in
    let indices = indicies_maker num_records in
    let offset = ref (sizeof_igmp3) in
    let get_gr = (fun i ->  let bits = Cstruct.shift bits (!offset) in
                            let gr = GroupRec.parse bits in
                            offset := (!offset + GroupRec.len gr);
                            gr) in
    let grs = List.map get_gr indices in
    { chksum = chksum; grs = grs}

  let len (msg: t) =
    let grs_len = List.fold_left (fun acc gr -> acc + (GroupRec.len gr)) 0 msg.grs in
    sizeof_igmp3 + grs_len

  (* Assumes that bits has enough room. *)
  let marshal (bits : Cstruct.t) (msg : t) =
    set_igmp3_chksum bits 0;
    set_igmp3_num_records bits (List.length msg.grs);
    let gr_bits = Cstruct.shift bits sizeof_igmp3 in
    ignore (List.fold_left GroupRec.marshal gr_bits msg.grs);
    (* ADF: hack since Igmp.sizeof_igmp not defined at this point *)
    let igmp_hdr = Cstruct.sub bits (-1) (1 + len msg) in
    let chksum = Tcpip_checksum.ones_complement igmp_hdr in
    set_igmp3_chksum bits chksum;

end

module Igmp = struct

  type msg =
    | Igmp1and2 of Igmp1and2.t
    | Igmp3 of Igmp3.t
    | Unparsable of (int8 * Cstruct.t)
  with sexp

  type t = {
    ver_and_typ : int8;
    msg : msg
  } with sexp

  cenum igmp_msg_type {
    IGMP_MSG_QUERY = 0x11;
    IGMP_v1_REPORT = 0x12;
    IGMP_v2_REPORT = 0x16;
    IGMP_v2_LEAVE = 0x17;
    IGMP_v3_REPORT = 0x22
  } as uint8_t

  cstruct igmp {
    uint8_t ver_and_typ (* version implicit in type. facepalm. *)
  } as big_endian

  let format_msg fmt = function
    | Igmp1and2 igmp1and2 -> Igmp1and2.format fmt igmp1and2
    | Igmp3 igmp3 -> Igmp3.format fmt igmp3
    | Unparsable (_, b) -> Format.fprintf fmt "msg_len=%d" (Cstruct.len b)

  let format_ver_and_typ fmt v =
    let open Format in
    match v with
      | 0x11 -> fprintf fmt "IGMP Membership Query";
      | 0x12 -> fprintf fmt "IGMP v1 Membership Report"
      | 0x16 -> fprintf fmt "IGMP v2 Membership Report"
      | 0x17 -> fprintf fmt "IGMP v2 Leave Group"
      | 0x22 -> fprintf fmt "IGMP v3 Membership Report"
      | n -> fprintf fmt "IGMP ver_and_type=%d" n

  let format fmt v =
    let open Format in
    fprintf fmt "@[%a@,%a@]"
      format_ver_and_typ v.ver_and_typ
      format_msg v.msg

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_igmp then
      raise (UnparsablePacket "not enough bytes for IGMP header");
    let ver_and_typ = get_igmp_ver_and_typ bits in
    let bits = Cstruct.shift bits sizeof_igmp in
    let msg =
      try match int_to_igmp_msg_type ver_and_typ with
        | Some IGMP_MSG_QUERY -> Igmp1and2 (Igmp1and2.parse bits)
        | Some IGMP_v1_REPORT -> Igmp1and2 (Igmp1and2.parse bits)
        | Some IGMP_v2_REPORT -> Igmp1and2 (Igmp1and2.parse bits)
        | Some IGMP_v2_LEAVE -> Igmp1and2 (Igmp1and2.parse bits)
        | Some IGMP_v3_REPORT -> Igmp3 (Igmp3.parse bits)
        | _ -> Unparsable (ver_and_typ, bits)
      with UnparsablePacket _ -> Unparsable (ver_and_typ, bits) in
    { ver_and_typ = ver_and_typ; msg = msg }

  let len (pkt: t) =
    let msg_len = match pkt.msg with
      | Igmp1and2 igmp1and2 -> Igmp1and2.len igmp1and2
      | Igmp3 igmp3 -> Igmp3.len igmp3
      | Unparsable (_, data) -> Cstruct.len data in
    sizeof_igmp + msg_len

  (* Assumes that bits has enough room. *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    set_igmp_ver_and_typ bits pkt.ver_and_typ;
    let bits = Cstruct.shift bits sizeof_igmp in
    match pkt.msg with
      | Igmp1and2 igmp1and2 ->
        Igmp1and2.marshal bits igmp1and2
      | Igmp3 igmp3 ->
        Igmp3.marshal bits igmp3
      | Unparsable (_, data) ->
        Cstruct.blit data 0 bits 0 (Cstruct.len data)
end

module Ip = struct

  type tp =
    | Tcp of Tcp.t
    | Udp of Udp.t
    | Icmp of Icmp.t
    | Igmp of Igmp.t
    | Unparsable of (nwProto * Cstruct.t)
  with sexp

  module Flags = struct
  (** [Flags] is the type of IPv4 flags. *)

    type t =
      { df : bool (** Don't fragment. *)
      ; mf : bool (** More fragments. *)
      } with sexp

    let to_string v = Printf.sprintf "{ df = %B; mf = %B }" v.df v.mf

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
    options : Cstruct.t;
    tp : tp
  } with sexp

  let format_tp fmt = function
    | Tcp tcp -> Tcp.format fmt tcp
    | Udp udp -> Udp.format fmt udp
    | Icmp icmp -> Icmp.format fmt icmp
    | Igmp igmp -> Igmp.format fmt igmp
    | Unparsable (proto, _) -> Format.fprintf fmt "protocol=%d" proto

  let format fmt v =
    let open Format in
    fprintf fmt "@[nwSrc=%s,nwDst=%s,%a@]"
      (string_of_ip v.src)
      (string_of_ip v.dst) 
      format_tp v.tp

  cenum ip_proto { 
    IP_ICMP = 0x01;
    IP_IGMP = 0x02;
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
    uint32_t dst
  } as big_endian

  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_ip then
      raise (UnparsablePacket "not enough bytes for IP header");
    let vhl = get_ip_vhl bits in 
    if vhl lsr 4 <> 4 then
      raise (UnparsablePacket "expected IPv4 header");
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
    let options_len = (ihl * 4) - sizeof_ip in
    let options = Cstruct.sub bits sizeof_ip options_len in
    let bits = Cstruct.shift bits (ihl * 4) in 
    let tp = 
      try match int_to_ip_proto proto with 
        | Some IP_ICMP -> Icmp (Icmp.parse bits)
        | Some IP_IGMP -> Igmp (Igmp.parse bits)
        | Some IP_TCP -> Tcp (Tcp.parse bits)
        | Some IP_UDP -> Udp (Udp.parse bits)
        | _ -> Unparsable (proto, bits) 
      with UnparsablePacket _ -> Unparsable (proto, bits) in
    { tos = tos;
      ident = ident;
      flags = flags;
      frag = frag;
      ttl = ttl;
      chksum = chksum;
      src = src;
      dst = dst;     
      options = options;
      tp = tp }

  let len (pkt : t) = 
    let options_len = Cstruct.len pkt.options in
    let tp_len = match pkt.tp with 
      | Tcp tcp -> Tcp.len tcp
      | Udp udp -> Udp.len udp
      | Icmp icmp -> Icmp.len icmp
      | Igmp igmp -> Igmp.len igmp
      | Unparsable (_, data) -> Cstruct.len data in 
    sizeof_ip + options_len + tp_len

  (* Assumes there is enough space *)
  let marshal (bits : Cstruct.t) (pkt:t) =
    let header_len = sizeof_ip + (Cstruct.len pkt.options) in
    let v = 4 in (* IP version 4. *)
    let ihl = header_len / 4 in
    let vhl = (v lsl 4) lor ihl in
    set_ip_vhl bits vhl;
    set_ip_tos bits pkt.tos;
    set_ip_len bits (len pkt);
    set_ip_ident bits pkt.ident;
    set_ip_frag bits (((Flags.to_int pkt.flags) lsl 13) lor pkt.frag);
    set_ip_ttl bits pkt.ttl;
    let proto = match pkt.tp with
      | Tcp _ -> tcp_code
      | Udp _ -> udp_code
      | Icmp _ -> icmp_code
      | Igmp _ -> igmp_code
      | Unparsable (p, _) -> p in
    set_ip_proto bits proto;
    set_ip_src bits pkt.src;
    set_ip_dst bits pkt.dst;
    Cstruct.blit pkt.options 0 bits sizeof_ip (Cstruct.len pkt.options);
    set_ip_chksum bits 0;
    let chksum = Tcpip_checksum.ones_complement (Cstruct.sub bits 0 header_len) in
    set_ip_chksum bits chksum;
    let bits = Cstruct.shift bits header_len in
    match pkt.tp with
      | Tcp tcp -> 
        Tcp.marshal bits tcp;
        Tcp.checksum bits pkt.src pkt.dst tcp
      | Udp udp ->
        Udp.marshal bits udp
      | Icmp icmp -> 
        Icmp.marshal bits icmp
      | Igmp igmp ->
        Igmp.marshal bits igmp
      | Unparsable (protocol, data) ->
        Cstruct.blit data 0 bits 0 (Cstruct.len data)

end

module Arp = struct

  type t =
    | Query of dlAddr * nwAddr * nwAddr
    | Reply of dlAddr * nwAddr * dlAddr * nwAddr
  with sexp

  let format fmt v =
    let open Format in
    match v with
      | Query (srcMac, srcIP,dstIP) ->
        (* src mac should be the same as ethernet srcMac, in theory *)
        fprintf fmt "@[ARP Query,senderIP=%s,targetIP=%s@]"
          (string_of_ip srcIP) (string_of_ip dstIP)
      | Reply (srcMac, srcIP, dstMac, dstIP) ->
        (* src mac should be the same as ethernet srcMac, in theory *)
        fprintf fmt "@[ARP Reply,senderMac=%s,senderIP=%s,targetIP=%s@]"
          (string_of_mac srcMac)
          (string_of_ip srcIP)
          (string_of_ip dstIP)
      

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
  
  let parse (bits : Cstruct.t) =
    if Cstruct.len bits < sizeof_arp then
      raise (UnparsablePacket "not enough bytes for ARP header");
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

  let len pk = sizeof_arp (* both requests and replies do have the same size *)

  (* Assumes there is enough space *)
  let marshal (bits : Cstruct.t) (pkt : t) =
    (* NOTE(ARJUN, JNF): ARP packets specify the size of L2 addresses, so 
       they can be used with IPv6. This version assumes we are doing IPv4. *)
    set_arp_htype bits 1;
    set_arp_ptype bits ip_code;
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
  | Unparsable of (dlTyp * Cstruct.t)
  with sexp

type packet = {
  dlSrc : dlAddr;
  dlDst : dlAddr; 
  dlVlan : dlVlan;
  dlVlanDei : dlVlanDei;
  dlVlanPcp : dlVlanPcp;
  nw : nw
} with sexp

let format_nw fmt v =
  let open Format in
  match v with 
    | Ip ip -> Ip.format fmt ip
    | Arp arp -> Arp.format fmt arp
    | Unparsable (typ, _) -> fprintf fmt "frameType=0x%X" typ

let format_vlan fmt v =
  let open Format in
  match v with
    | None -> ()
    | Some x -> fprintf fmt "vlan=%d," x

let format_packet fmt v =
  let open Format in
  fprintf fmt "@[dlSrc=%s,dlDst=%s,%a%a@]"
    (string_of_mac v.dlSrc)
    (string_of_mac v.dlDst)
    format_vlan v.dlVlan
    format_nw v.nw

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
      | Ip.Udp _ -> 17
      | Ip.Icmp _ -> 1
      | Ip.Igmp _ -> 2
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
    | Ip.Udp frg -> frg.Udp.src
    | Ip.Icmp _ -> raise (Invalid_argument "tpSrc: ICMP packet")
    | Ip.Igmp _ -> raise (Invalid_argument "tpSrc: IGMP packet")
    | Ip.Unparsable _ -> 
      raise (Invalid_argument "tpSrc: cannot parse body of IP packet"))
  | Arp _ -> raise (Invalid_argument "tpSrc: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "tpSrc: unparsable packet")

let tpDst pkt = match pkt.nw with 
  | Ip ip ->
    (match ip.Ip.tp with
    | Ip.Tcp frg -> frg.Tcp.dst
    | Ip.Udp frg -> frg.Udp.dst
    | Ip.Icmp _ -> raise (Invalid_argument "tpDst: ICMP packet")
    | Ip.Igmp _ -> raise (Invalid_argument "tpDst: IGMP packet")
    | Ip.Unparsable _ -> 
      raise (Invalid_argument "tpDst: cannot parse body of IP packet"))
  | Arp _ -> raise (Invalid_argument "tpDst: ARP packet")
  | Unparsable _ -> raise (Invalid_argument "tpDst: unparsable packet")

let arpOperation pkt = match pkt.nw with
  | Arp (Arp.Query _) -> 1
  | Arp (Arp.Reply _) -> 2
  | Ip _ -> raise (Invalid_argument "arpOperation: IP packet")
  | Unparsable _ -> raise (Invalid_argument "arpOperation: unparsable packet")

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
    Ip.Tcp { tcp with Tcp.src = src }
  | Ip.Udp udp ->
    Ip.Udp { udp with Udp.src = src }
  | tp -> 
    tp

let tp_setTpDst tp dst = match tp with 
  | Ip.Tcp tcp ->
    Ip.Tcp { tcp with Tcp.dst = dst }
  | Ip.Udp udp ->
    Ip.Udp { udp with Udp.dst = dst }
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

let get_dlTyp nw = match nw with
    | Ip _ -> ip_code
    | Arp _ -> arp_code
    | Unparsable (t, _) -> t

let dlTyp pkt = get_dlTyp pkt.nw

let string_of_dlAddr = string_of_mac

let string_of_dlTyp v = match v with
  | 0x800 -> "ip"
  | 0x806 -> "arp"
  | v' -> Printf.sprintf "0x%x" v'

let string_of_dlVlan = function
  | None -> "none"
  | Some n -> string_of_int n

let string_of_dlVlanPcp = string_of_int

let string_of_dlVlanDei = string_of_bool

let string_of_nwAddr = string_of_ip

let string_of_nwProto = function
  | 0x01 -> "icmp"
  | 0x02 -> "igmp"
  | 0x06 -> "tcp"
  | 0x11 -> "udp"
  | v -> string_of_int v

let string_of_nwTos = string_of_int

let string_of_tpPort = string_of_int

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
  uint16_t tag; (* pcp, dei, and tag *)
  uint16_t typ
} as big_endian

cenum eth_typ {
  ETHTYP_IP = 0x0800;
  ETHTYP_ARP = 0x0806;
  ETHTYP_VLAN = 0x8100
} as uint16_t

let vlan_mask = 0xfff
let vlan_pcp_mask = 0x7 lsl 13
let vlan_dei_mask = 0x1000

(* Transport *)

cstruct udp { 
  uint16_t src;
  uint16_t dst;
  uint16_t len;
  uint16_t chksum
} as big_endian

(* TODO(arjun): error if not enough space (annoying to do due to VLANs)*)
let parse (bits : Cstruct.t) =
  let src = Cstruct.to_string (get_eth_src bits) in
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let typ = get_eth_typ bits in 
  let (vlan_tag, vlan_dei, vlan_pcp, typ, offset) =
    match int_to_eth_typ typ with 
      | Some ETHTYP_VLAN -> 
        let tag_and_pcp = get_vlan_tag bits in 
        let vlan_tag = tag_and_pcp land 0xfff in 
        let vlan_dei = (tag_and_pcp land 0x1000) > 0 in
        let vlan_pcp = tag_and_pcp lsr 13 in 
        let typ = get_vlan_typ bits in 
        (Some vlan_tag, vlan_dei, vlan_pcp, typ, sizeof_vlan)
      | _ -> 
        (None, false, 0x0, typ, sizeof_eth) in
  let bits = Cstruct.shift bits offset in
  let nw_header = 
    try match int_to_eth_typ typ with 
      | Some ETHTYP_IP -> Ip (Ip.parse bits)
      | Some ETHTYP_ARP -> 
        begin try 
                Arp (Arp.parse bits)
          with UnparsablePacket _ -> 
            Unparsable (typ, Cstruct.of_string (Cstruct.to_string bits))
      end
      | _ -> 
        Unparsable (typ, Cstruct.of_string (Cstruct.to_string bits))
    with UnparsablePacket _ ->
      Unparsable (typ, Cstruct.of_string (Cstruct.to_string bits)) in
  { dlSrc = mac_of_bytes src;
    dlDst = mac_of_bytes dst;
    dlVlan = vlan_tag;
    dlVlanDei = vlan_dei;
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

let string_of_mk formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let to_string = string_of_mk format_packet

let marshal_helper (bits : Cstruct.t) (pkt : packet) =
  set_eth_src (bytes_of_mac pkt.dlSrc) 0 bits;
  set_eth_dst (bytes_of_mac pkt.dlDst) 0 bits;
  let dlTyp = get_dlTyp pkt.nw in
  let bits =
    match pkt.dlVlan with
      | Some v ->
        set_vlan_hdr bits 0x8100;
        let tag = (if pkt.dlVlanDei then vlan_dei_mask else 0x0)
                    lor (pkt.dlVlanPcp lsl 13) lor v in
        set_vlan_tag bits tag;
        set_vlan_typ bits dlTyp;
        Cstruct.shift bits sizeof_vlan
      | None ->
        set_eth_typ bits dlTyp;
        Cstruct.shift bits sizeof_eth in
  match pkt.nw with 
    | Ip ip -> Ip.marshal bits ip
    | Arp arp -> Arp.marshal bits arp
    | Unparsable (_, data) -> Cstruct.blit data 0 bits 0 (Cstruct.len data)

let marshal (pkt:packet) : Cstruct.t = 
  (* Allocating (len pkt) ensures the other marshalers have enough room *)
  let bits = Cstruct.create (len pkt) in 
  let () = marshal_helper bits pkt in 
  bits

let ip_of_string (s : string) : nwAddr =
  let b = Str.split (Str.regexp "\\.") s in
  let open Int32 in
      (logor (shift_left (of_string (List.nth b 0)) 24)
         (logor (shift_left (of_string (List.nth b 1)) 16)
            (logor (shift_left (of_string (List.nth b 2)) 8)
               (of_string (List.nth b 3)))))

let mac_of_string (s : string) : dlAddr =
  let b = Str.split (Str.regexp ":") s in
  let parse_byte str = Int64.of_string ("0x" ^ str) in
  let open Int64 in
      (logor (shift_left (parse_byte (List.nth b 0)) 40)
         (logor (shift_left (parse_byte (List.nth b 1)) 32)
            (logor (shift_left (parse_byte (List.nth b 2)) 24)
               (logor (shift_left (parse_byte (List.nth b 3)) 16)
                  (logor (shift_left (parse_byte (List.nth b 4)) 8)
                     (parse_byte (List.nth b 5)))))))

let ipv6_of_string (s : string) : ipv6Addr =
  let b = Str.split (Str.regexp ":") s in
  let bytes_len = List.length b in
  let rec fill_with_0 n =
    if n = 0 then ["0"]
    else "0"::(fill_with_0 (n-1)) in
  let rec fill_bytes b =
    match b with
      | [] -> []
      | ""::q -> List.append (fill_with_0 (8-bytes_len)) q
      | t::q -> t::(fill_bytes q) in
  let b = fill_bytes b in
  let parse_byte str = Int64.of_string ("0x" ^ str) in
  let open Int64 in
      (logor (shift_left (parse_byte (List.nth b 0)) 48)
         (logor (shift_left (parse_byte (List.nth b 1)) 32)
            (logor (shift_left (parse_byte (List.nth b 2)) 16)
               (parse_byte (List.nth b 3))))),
      (logor (shift_left (parse_byte (List.nth b 4)) 48)
         (logor (shift_left (parse_byte (List.nth b 5)) 32)
            (logor (shift_left (parse_byte (List.nth b 6)) 16)
               (parse_byte (List.nth b 7)))))
                        
