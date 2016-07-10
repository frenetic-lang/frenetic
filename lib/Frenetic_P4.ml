open Core.Std
open Frenetic_Fdd
open Frenetic_NetKAT

module Compiler = Frenetic_NetKAT_Compiler
module FieldTable : Hashtbl.S with type key = Field.t = Hashtbl.Make(Field)

type fdd_path = Field.t list * Action.t
type parser =
  | Eth
  | Ip
  | Tcp
  | Udp
  | Icmp
  | Igmp1and2
  | Igmp3
  | Igmp
  | Arp
  | VLAN
      [@@deriving show]

type table = int * Field.t list * action list
and action =
      | Actions of Action.t
      | Jump of table

type p4 = table * parser list * table list

let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

let tables = FieldTable.create ~size:15 ()
let table_id = ref 0

let compare_actions a a' = match a,a' with
  | Actions _, Jump _
  | Jump _, Actions _ -> 1
  | Actions a1, Actions a2 -> Action.compare a1 a2
  | Jump(id1,_,_), Jump(id2,_,_) -> compare id1 id2

let field_of_node node = match FDD.unget node with
  | FDD.Branch ((f,_),_,_) -> f
  | FDD.Leaf _ -> failwith "Cannot get field from a leaf node"

let rec traverse node fields : fdd_path list =
  match FDD.unget node with
  | FDD.Branch ((v,l), t, f) ->
    let fields' = begin match List.hd fields with
      | Some field -> if field = v then fields else v::fields
      | None -> [v] end in
    let true_paths  = traverse t fields' in
    let false_paths = traverse f fields' in
    List.unordered_append true_paths false_paths
  | FDD.Leaf a ->
    [ (List.rev fields,a) ]

let update_table field action tbl_opt = match tbl_opt with
  | None ->
    table_id := !table_id + 1;
    (!table_id, [ field ], [action])
  | Some (id,_,actions) ->
    (id, [field], List.dedup ~compare:compare_actions (action::actions) )

let process_path (p:fdd_path) =
  let rec aux (fields,action) = match fields with
  | [] -> failwith "Empty Fdd path"
  | [f] ->
    FieldTable.update tables f ~f:(update_table f (Actions action ));
    f
  | first::rest ->
    let next = aux (rest, action) in
    let jump = Jump (FieldTable.find_exn tables next) in
    FieldTable.update tables first ~f:(update_table first jump);
    first in
  ignore(aux p)


let p4_of_policy (pol:policy) : p4 =
  let fdd = compile_local pol in
  let paths = traverse fdd [] in
  let start = field_of_node fdd in
  print_endline (FDD.to_dot fdd);
  List.iter paths ~f:process_path;
  let parsers,tbls = FieldTable.fold tables ~init:([],[])
      ~f:(fun  ~key ~data (parsers,tables) -> match key with
      | Vlan
      | VlanPcp -> ( VLAN::parsers, data::tables )
      | EthType
      | EthSrc
      | EthDst -> ( Eth::parsers, data::tables )
      | IP4Src
      | IP4Dst
      | IPProto -> ( Eth::Ip::parsers, data::tables)
      | TCPSrcPort
      | TCPDstPort -> ( Eth::Ip::Tcp::parsers, data::tables )
      | Location
      | Switch
      | VSwitch
      | VPort
      | VFabric
      | Channel ->
        let id,_,_ = data in
        printf "Unsupported match field:%s for table %d\n"
          (Field.to_string key) id;
        (parsers,tables)) in
  (FieldTable.find_exn tables start, List.dedup parsers, tbls)

let string_of_action a : string = match a with
  | Actions a -> Action.to_string a
  | Jump (id,_,_) -> sprintf "Jump:%d" id

let string_of_table (id,fs,acts) : string =
  let fields = String.concat ~sep:";" (List.map fs Field.to_string) in
  let actions = String.concat ~sep:";" (List.map acts string_of_action) in
  sprintf "Id:%d\tFields:[%s]\tActions:[%s]\n" id fields actions

let string_of_p4 (parsers, tables) =
  let open Buffer in
  let buffer = create 1000 in
  add_string buffer "\nParsers :";
  List.iter parsers ~f:(fun p ->
      add_string buffer "|" ;
      add_bytes buffer (show_parser p) );
  add_string buffer "\nTables:\n";
  List.iter tables ~f:(fun t ->
      add_string buffer (string_of_table t);
      add_string buffer "\n" );
  contents buffer

let header_types = Hashtbl.Poly.of_alist_exn
    [ (Eth,
       "header_type ethernet_t {
    fields {
        dstAddr : 48;
        srcAddr : 48;
        etherType : 16;
    }
}");
      (Arp,
       " header_type arp_t {
    fields {
        hrd : 16;
        pro : 16;
        hln : 8;
        pln : 8;
        op  : 16;
        sha : 48;
        spa : 32;
        tha : 48;
        tpa : 32;
    }
}");
      (Ip,
       "header_type ipv4_t {
    fields {
        version : 4;
        ihl : 4;
        diffserv : 8;
        totalLen : 16;
        identification : 16;
        flags : 3;
        fragOffset : 13;
        ttl : 8;
        protocol : 8;
        hdrChecksum : 16;
        srcAddr : 32;
        dstAddr: 32;
    }
}");
      (Udp,
       "header_type udp_t {
    fields {
        srcPort : 16;
        dstPort : 16;
        length_ : 16;
        checksum : 16;
    }
}");
      (Tcp,
       "
header_type tcp_t {
    fields {
        srcPort : 16;
        dstPort : 16;
        seqNo : 32;
        ackNo : 32;
        dataOffset : 4;
        res : 3;
        ecn : 3;
        urg : 1;
        ack : 1;
        push : 1;
        rst : 1;
        syn : 1;
        fin : 1;
        window : 16;
        checksum : 16;
        urgentPtr : 16;
    }
}
#define TCP_OPTIONS_END 0x00
header_type options_end_t {
        fields {
                kind :8;
        }
}
#define TCP_OPTIONS_NOP 0x01
header_type options_nop_t {
   fields {
        kind: 8;
    }
}
#define TCP_OPTIONS_MSS 0x02
header_type options_mss_t {
   fields {
        kind : 8;
        len : 8;
        MSS : 16;
   }
}
#define TCP_OPTIONS_WSCALE 0x03
header_type options_wscale_t {
   fields {
        kind : 8;
        len : 8;
        wscale : 8;
   }
}

#define TCP_OPTIONS_SACK 0x04
header_type options_sack_t {
   fields {
        kind: 8;
        len : 8;
   }
}
#define TCP_OPTIONS_TS 0x08
header_type options_ts_t {
   fields {
        kind: 8;
        len : 8;
        ttee : 64;
   }
}
");
    ]

let headers = Hashtbl.Poly.of_alist_exn
    [ (Eth, "header ethernet_t ethernet;");
      (Ip, "header ipv4_t ipv4;");
      (Arp, "header arp_t arp;");
      (Udp, "header udp_t udp;");
      (Tcp, String.concat ~sep:"\n"
         ["header tcp_t tcp;";
          "header options_end_t options_end;";
          "header options_nop_t options_nop[3];";
          "header options_mss_t options_mss;";
          "header options_wscale_t options_wscale";
          "header options_sack_t options_sack";
          "header options_ts_t options_ts; "
         ]);
    ]

let parsers = Hashtbl.Poly.of_alist_exn
    [ ( Eth,
"parser parse_ethernet {
    extract(ethernet);
    return select(latest.etherType) {
        0x806: parse_arp;
        0x0800: parse_ipv4;
        default: ingress;
    }
}");
      ( Arp,
"
parser parse_arp {
    extract(arp);
    return ingress;
}");

      ( Ip,
"parser parse_ipv4 {
    extract(ipv4);
    return select(latest.protocol) {
        0x11 : parse_udp;
        6 : parse_tcp;
        default: ingress;
    }
}
");
      ( Tcp,
"header_type my_metadata_t {
    fields {
        parse_tcp_options_counter : 8;
    }
}
metadata my_metadata_t my_metadata;

parser parse_tcp {
    extract(tcp);
    set_metadata(my_metadata.parse_tcp_options_counter, tcp.dataOffset * 4 - 20);
    return select(latest.syn) {
        1: parse_tcp_options;
        default : ingress;
    }
}

parser parse_tcp_options {
        return select(my_metadata.parse_tcp_options_counter, current(0,8)) {
                0x0000 mask 0xff00 : ingress;
                0x0000 mask 0x00ff : parse_end;
                0x0001 mask 0x00ff : parse_nop;
                0x0002 mask 0x00ff : parse_mss;
                0x0003 mask 0x00ff : parse_wscale;
                0x0004 mask 0x00ff : parse_sack;
                0x0008 mask 0x00ff : parse_ts;
        }
}

parser parse_end {
        extract(options_end);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-1);
        return parse_tcp_options;
}

parser parse_nop {
        extract(options_nop[next]);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-1);
        return parse_tcp_options;
}

parser parse_mss {
        extract(options_mss);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-4);
        return parse_tcp_options;
}

parser parse_wscale {
        extract(options_wscale);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-3);
        return parse_tcp_options;
}

parser parse_sack {
        extract(options_sack);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-2);
        return parse_tcp_options;
}

parser parse_ts {
        extract(options_ts);
        set_metadata(my_metadata.parse_tcp_options_counter, my_metadata.parse_tcp_options_counter-10);
        return parse_tcp_options;
}

");
    ]


let code_of_parser p =
  let find = Hashtbl.Poly.find in
  match find header_types p, find headers p with
  | Some ht, Some h -> String.concat ~sep:"\n" [ht;h]
  | _ -> sprintf "Missing header or header type for %s" (show_parser p)

let code_of_field (c:Field.t) = match c with
  | EthType -> "ethernet.etherType : exact;"
  | EthSrc  -> "ethernet.srcAddr : exact;"
  | EthDst -> "ethernet.dstAddr : exact;"
  | IP4Src -> "ipv4.srcAddr : exact;"
  | IP4Dst -> "ipv4.dstAddr : exact;"
  | IPProto -> "ipv4.protocol : exact;"
  | TCPSrcPort -> "tcp.srcPort : exact;"
  | TCPDstPort -> "tcp.dstPort : exact;"
  | _ -> ""

let code_of_define_action a = match a with
  | Jump (id,_,_) ->
    sprintf "action goto_tbl_%d { nop }" id
  | _ -> ""

let code_of_do_action a = match a with
  | Actions a -> Action.to_string a
  | Jump (id,_,_) -> sprintf "goto_tbl_%d" id

let code_of_table (id,fs,acts) =
  let fields = String.concat ~sep:"\n" (List.map fs ~f:code_of_field) in
  let actions = String.concat  ~sep:"\n" (List.dedup (List.map acts
                                                        ~f:code_of_do_action))
  in
  let gotos = List.map acts ~f:code_of_define_action in
  let code = sprintf
    "table tbl_%d {
    reads {
         %s
    }
    actions {
         %s
    }
}
" id fields actions in
  (code, gotos)

let control_code t =
  let rec apply (id,fs,acts) =
    let start = sprintf "apply(tbl_%d)" id in
    let jumps = List.filter acts ~f:(fun a -> match a with
        | Actions _ -> false
        | Jump _ -> true) in
    match jumps with
    | [] -> (start ^ ";")
    | js ->
      let gotos = List.fold js ~init:["};"] ~f:(fun acc action -> match action with
          | Actions _ -> acc
          | Jump ((id,_,_) as t) ->
            (sprintf "goto_tbl_%d { %s };" id (apply t) )::acc) in
      String.concat ~sep:"\n" (start::" {"::gotos)
  in
  sprintf "control ingress { %s }" (apply t)

(* Expected control block: *)
(* control ingress { *)
(*     apply(tbl_2) *)
(*       { goto_tbl_1 { *)
(*             apply(tbl_1); *)
(*       }} *)
(* } *)

let flatten lss =
  List.fold lss ~init:[] ~f:(fun acc ls ->
      List.fold ls ~init:acc ~f:(fun acc l -> l::acc))

let code_of_p4 (start, parsers, tables) =
  let ps = String.concat ~sep:"\n" (List.map parsers ~f:code_of_parser) in
  let tables, gotos = List.unzip (List.map tables ~f:code_of_table) in
  let gotos = String.concat ~sep:"\n" (List.dedup (flatten gotos)) in
  let tbls = String.concat ~sep:"\n" tables in
  let ctrl = control_code start in
  String.concat ~sep:"\n\n" [ps ; gotos; tbls; ctrl]
