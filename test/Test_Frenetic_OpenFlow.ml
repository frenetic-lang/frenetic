open QuickCheck
open Core
open Frenetic_OpenFlow

module Gen = QuickCheck_gen

module LatticeTest(L : sig
  type t
  val arbitrary_t : t arbitrary

  val match_all : t

  val less_eq : t -> t -> bool
  val eq : t -> t -> bool
  val join : t -> t -> t

  val string_of : t -> string
end) = struct

  let t_quickCheck prop =
    let test = testable_fun L.arbitrary_t L.string_of testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let t2_quickCheck prop =
    let arb =
      let open Gen in
      L.arbitrary_t >>= fun p1 ->
      L.arbitrary_t >>= fun p2 ->
        ret_gen (p1, p2) in
    let show (p1, p2) =
      Printf.sprintf "%s, %s" (L.string_of p1) (L.string_of p2) in
    let test = testable_fun arb show testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let t3_quickCheck prop =
    let arb =
      let open Gen in
      L.arbitrary_t >>= fun p1 ->
      L.arbitrary_t >>= fun p2 ->
      L.arbitrary_t >>= fun p3 ->
        ret_gen (p1, p2, p3) in
    let show (p1, p2, p3) =
      Printf.sprintf "%s, %s, %s"
        (L.string_of p1) (L.string_of p2) (L.string_of p3) in
    let test = testable_fun arb show testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let implies a b = b || (not a)

  open L

  let%test "eq reflexive: eq p p" =
    let prop_eq_reflexive p =
      eq p p in
    t_quickCheck prop_eq_reflexive

  let%test "eq symmetric: eq p1 p2 <=> eq p2 p1" =
    let prop_eq_symmetric (p1, p2) =
      eq p1 p2 = eq p2 p1 in
    t2_quickCheck prop_eq_symmetric

  let%test "eq transitive: eq p1 p2 && eq p2 p3 => eq p1 p3" =
    let prop_eq_transitive (p1, p2, p3) =
      implies (eq p1 p2 && eq p2 p3) (eq p1 p3) in
    t3_quickCheck prop_eq_transitive

  let%test "less_eq reflexivity: less_eq p p" =
    let prop_reflexive p = less_eq p p = true in
    t_quickCheck prop_reflexive

  let%test "less_eq antisymmetry: less_eq p1 p2 && less_eq p2 p1 <=> p1 = p2" =
    let prop_antisymmetry (p1, p2) =
      (less_eq p1 p2 && less_eq p2 p1) = (eq p1 p2) in
    t2_quickCheck prop_antisymmetry

  let%test "less_eq transitivity: less_eq p1 p2 && less_eq p2 p3 => less_eq p1 p3" =
    let prop_transitivity (p1, p2, p3) =
      implies (less_eq p1 p2 && less_eq p2 p3) (less_eq p2 p3) in
    t3_quickCheck prop_transitivity

  let%test "less_eq top: less_eq p match_all" =
    let prop_top p =
      less_eq p match_all in
    t_quickCheck prop_top

  let%test "join symmetry: join p1 p2 <=> join p2 p1" =
    let prop_symmetry (p1, p2) = eq (join p1 p2) (join p2 p1) in
    t2_quickCheck prop_symmetry

  let%test "join exact: less_eq p1 (join p1 p2) && less_eq p2 (join p1 p2)" =
    let prop_exact (p1, p2) =
      less_eq p1 (join p1 p2) && less_eq p2 (join p1 p2) in
    t2_quickCheck prop_exact

  let%test "join least: less_eq p1 p3 && less_eq p2 p3 <=> less_eq (join p1 p2) p3" =
    let prop_least (p1, p2, p3) =
      (less_eq p1 p3 && less_eq p2 p3) = (less_eq (join p1 p2) p3) in
    t3_quickCheck prop_least

  let%test "join comparable least: less_eq p1 p2 <=> join p1 p2 = p2" =
    (* This is the same as "join least" when p2 = p3 *)
    let prop_comparable_least (p1, p2) =
      (less_eq p1 p2) = (eq (join p1 p2) p2) in
    t2_quickCheck prop_comparable_least
      
  let%test "eq partial: eq p1 p2 <=> less_eq p1 p2 && less_eq p2 p1" =
    let prop_eq_partial (p1, p2) =
      eq p1 p2 = (less_eq p1 p2 && less_eq p2 p1) in
    t2_quickCheck prop_eq_partial

end

(** Packet forwarding *)

let%test_module _ = (module struct
  open Pattern.Ip

  let%test "Pattern.Ip.match_all returns a totally zero pattern" =
    match_all = (0l, 0l) 

  let%test "Pattern.Ip.less_eq returns false if there are less mask bits in first" =
    not (less_eq (0x10203040l, 16l) (0x10203040l, 24l))

  let%test "Pattern.Ip.less_eq returns true if p1/m1 matches less addresses in same net than p2/m2" =
    (* This works because a mask of 24 bits leaves 8 bits for the host = 256 hosts.
       A mask of 16 bits leaves 16 bits for the hosts = 65,636 hosts.  So p1 <= p2 *)
    less_eq (0x10203040l, 24l) (0x10203040l, 16l)

  let%test "Pattern.Ip.less_eq returns false (incomparable) if p1/m1 and p2/m2 are two different networks" =
    not (less_eq (0x10203000l, 24l) (0x1020aa00l, 24l))

  let%test "Pattern.Ip.eq returns false if the number of mask bits differ" =
    not (eq (0x10203040l, 16l) (0x10203040l, 24l))

  let%test "Pattern.Ip.eq returns true if the patterns agree to the number of mask bits" =
    eq (0x10203040l, 24l) (0x102030aal, 24l)

  let%test "Pattern.Ip.join returns the least restrictive (more addresses) if they point at same network" =
    join (0x10203040l, 16l) (0x10203040l, 24l) = (0x10203040l, 16l)

  let%test "Pattern.Ip.join returns the largest network that both patterns belong to" =
    (* The patterns agree for 9 bits.  Note that there are actually many equivalent answers to this
    query - any pattern (0x10nnnnnn, 9) will do *)
    join (0x10203040l, 16l) (0x10506070l, 24l) = (0x10203040l, 9l)

  let%test "Pattern.Ip.join returns total wildcard if they don't belong to any common network." =
    join (0x10203040l, 16l) (0xef0e0d0cl, 16l) = (0l, 0l)

  let%test "Pattern.Ip.intersect returns the most restrictive of two patterns on the same net " =
    intersect (0x10203040l, 24l) (0x10203040l, 16l) = Some (0x10203040l, 24l)

  let%test "Pattern.Ip.intersect returns None if the patterns are not comparable (not on same net)" =
    intersect (0x10203040l, 16l) (0xef0e0d0cl, 16l) = None

  let%test "Pattern.Ip.compatible returns true if two patterns are on the same net " =
    compatible (0x10203040l, 24l) (0x10203040l, 16l) 

  let%test "Pattern.Ip.compatible returns false if the patterns are not comparable (not on same net)" =
    not (compatible (0x10203040l, 16l) (0xef0e0d0cl, 16l))

  let%test "Pattern.Ip.compatible returns false if the patterns are not comparable (not on same net)" =
    not (compatible (0x10203040l, 16l) (0xef0e0d0cl, 16l))

  let%test "Pattern.Ip.string_of returns human readable pattern with mask if it's less than all 1 bits" =
    string_of (0x10203040l, 16l) = "16.32.48.64/16"

  let%test "Pattern.Ip.string_of returns human readable pattern without mask if it's all 1 bits" =
    string_of (0x10203040l, 32l) = "16.32.48.64"
end)

(* Take advantage of the partial ordering in the lattice to verify mathematical properties *)
module Ip = LatticeTest(struct
  include Frenetic_OpenFlow.Pattern.Ip
  let arbitrary_t = Arbitrary_Frenetic_OpenFlow.arbitrary_ip_mask
end)

let%test_module _ = (module struct
  open Pattern

  let all_http = { match_all with tpSrc = Some 80; }
  let all_http_from_private_net = { match_all with tpSrc = Some 80; nwSrc = Some (0xc0a80000l, 24l) }
  let all_http_from_larger_private_net = { match_all with tpSrc = Some 80; nwSrc = Some (0xc0a80000l, 16l) }
  let all_https = { match_all with tpSrc = Some 443; }

  let%test "Pattern.match_all returns a totally zero pattern" =
    match_all.nwSrc = None  (* For instance *)

  let%test "Pattern.less_eq returns true if they're the same pattern" =
    less_eq all_http all_http

  let%test "Pattern.less_eq returns true if p1 matches less packets than p2" =
    less_eq all_http_from_private_net all_http

  let%test "Pattern.less_eq returns true if p1 matches less packets than p2" =
    less_eq all_http_from_private_net all_http_from_larger_private_net 

  let%test "Pattern.less_eq returns false (incomparable) if p1 and p2 don't overlap at all" =
    not (less_eq all_http all_https)

  let%test "Pattern.eq returns false if the number of mask bits differ" =
    not (eq all_http_from_private_net all_http_from_larger_private_net)

  let%test "Pattern.eq returns true if the patterns agree" =
    eq all_http all_http

  let%test "Pattern.eq returns true if the patterns don't agree" =
    not (eq all_http all_https)

  let%test "Pattern.join returns the least restrictive pattern" =
    join all_http all_http_from_private_net = all_http

  let%test "Pattern.join returns the pattern with the largest network that both patterns belong to" =
    join all_http_from_private_net all_http_from_larger_private_net = all_http_from_larger_private_net

  let%test "Pattern.join returns total wildcard if patterns have no overlap" =
    join all_http all_https = match_all

  (* Note intersect and compatible are not really useful in this context, though we could define them *)

  let%test "Pattern.string_of returns human readable pattern" =
    string_of all_http_from_private_net = "{ipSrc=192.168.0.0/24,tcpSrcPort=80}"

  let%test "Pattern.string_of returns human readable pattern" =
    string_of match_all = "{}"
end)

module Pattern = LatticeTest(struct
  include Frenetic_OpenFlow.Pattern
  let arbitrary_t = Arbitrary_Frenetic_OpenFlow.arbitrary_pattern
end)

let%test "format_list formats empty list as []" = 
  format_list ~to_string:Int32.to_string [] = "[]"

let%test "format_list formats populated list as comma-separated [n1,n2,n3]" = 
  format_list ~to_string:Int32.to_string [1l; 2l; 3l] = "[1,2,3]"  

let fmt = Format.str_formatter

let output_to_port_3_action = Output(Physical(3l))
let output_to_port_3_action_string = "Output(3)"
let fast_fail_action = FastFail(9999l)
let fast_fail_action_string = "FastFail(9999)"
let drop_action = Output(Physical(0l))
let drop_action_string = "Output(0)"

let%test "format_action handles Output" =
  format_action fmt output_to_port_3_action;
  (Format.flush_str_formatter ()) = output_to_port_3_action_string

let%test "format_action handles Output to controller" =
  format_action fmt (Output(Controller(128)));
  (Format.flush_str_formatter ()) = "Output(Controller(128))"

let%test "format_action handles Enqueue" =
  format_action fmt (Enqueue(80l,90l));
  (Format.flush_str_formatter ()) = "Enqueue(80,90)"

let%test "format_action handles modifications" =
  format_action fmt (Modify(SetTCPDstPort(443)));
  (Format.flush_str_formatter ()) = "SetField(tcpDstPort, 443)"

let%test "format_action handles modifications of IP addresses" =
  format_action fmt (Modify(SetIP4Src(0xc0a80000l)));
  (Format.flush_str_formatter ()) = "SetField(ipSrc, 192.168.0.0)"

let%test "format_action handles fast fail groups" =
  format_action fmt fast_fail_action;
  (Format.flush_str_formatter ()) = fast_fail_action_string

let sample_sequence = [ output_to_port_3_action; fast_fail_action ]
let sample_sequence_string = output_to_port_3_action_string ^ "; " ^ fast_fail_action_string

let%test "format_seq formats sequences of actions in a ;-separated list" =
  format_seq fmt sample_sequence;
  (Format.flush_str_formatter ()) = sample_sequence_string

let%test "format_seq doesn't output anything for null sequence" =
  format_seq fmt [ ];
  (Format.flush_str_formatter ()) = ""

let sample_action_bucket = [ sample_sequence; [ drop_action ] ]
let sample_action_bucket_string = sample_sequence_string ^ " | " ^ drop_action_string 

let%test "format_par outputs action bucket in a | separated list" =
  format_par fmt sample_action_bucket;
  (Format.flush_str_formatter ()) = sample_action_bucket_string

let%test "format_par doesn't output anything for null action bucket" =
  format_par fmt [ ];
  (Format.flush_str_formatter ()) = ""

let sample_group = [ sample_action_bucket; [ [ Enqueue(3l, 4l) ]; [ Modify(SetIP4Src(0xc0a80000l)) ] ] ]
let sample_group_string = sample_action_bucket_string ^ " +\nEnqueue(3,4) | SetField(ipSrc, 192.168.0.0)"

let%test "format_group outputs group of action buckets in a + separated list" =
  format_group fmt sample_group;
  (Format.flush_str_formatter ()) = sample_group_string 

let%test "format_group doesn't output anything for null group of action buckets" =
  format_group fmt [ ];
  (Format.flush_str_formatter ()) = ""

open Frenetic_OpenFlow.Pattern
let sample_flow = { 
  pattern = { match_all with tpSrc = Some 80; nwSrc = Some (0xc0a80000l, 24l) };
  action = sample_group;
  cookie = 32249364871L;
  idle_timeout = Permanent;
  hard_timeout = ExpiresAfter(999)
}
let sample_flow_string = 
  "{pattern={ipSrc=192.168.0.0/24,tcpSrcPort=80},\n" ^
  (* This is almost like sample_group_string, but there's some alignment going on *)
  "action=Output(3); FastFail(9999) | Output(0) +\n" ^
  "       Enqueue(3,4) | SetField(ipSrc, 192.168.0.0),\n" ^
  "cookie=32249364871,idle_timeout=Permanent,hard_timeout=ExpiresAfter(999)}"

let%test "format_flow outputs a rule from the flow table" =
  format_flow fmt sample_flow;
  (Format.flush_str_formatter ()) = sample_flow_string

let sample_flow_table = [ 
  sample_flow; 
  { pattern = match_all ; action = [[[ drop_action ]]]; cookie = 0L; idle_timeout = Permanent; hard_timeout = Permanent }
]
(* Same thing as above - the alignment is different than sample_flow_string *)
let sample_flow_table_string = 
  "[{pattern={ipSrc=192.168.0.0/24,tcpSrcPort=80},\n" ^ 
  " action=Output(3); FastFail(9999) | Output(0) +\n" ^ 
  "        Enqueue(3,4) | SetField(ipSrc, 192.168.0.0),\n" ^ 
  " cookie=32249364871,idle_timeout=Permanent,hard_timeout=ExpiresAfter(999)}\n" ^ 
  "{pattern={},action=Output(0),cookie=0,idle_timeout=Permanent,\n" ^ 
  "hard_timeout=Permanent}]"

let%test "format_flowTable outputs the whole enchilada" =
  format_flowTable fmt sample_flow_table;
  (Format.flush_str_formatter ()) = sample_flow_table_string

(* These are just wrappers for their formatter counterparts *)

let%test "string_of_action handles Output" =
  string_of_action output_to_port_3_action = output_to_port_3_action_string

let%test "string_of_seq handles sequences" =
  string_of_seq sample_sequence = sample_sequence_string
  
let%test "string_of_par handles buckets" =
  string_of_par sample_action_bucket = sample_action_bucket_string

let%test "string_of_flow handles flow table entries" =
  string_of_flow sample_flow = sample_flow_string

(* Except string_of_flowTable which is much more complex *)

let%test "string_of_flowTable handles flow tables" =
  string_of_flowTable sample_flow_table = 
    "+----------------------------------------------------------+\n" ^
    "|  | Pattern              | Action                         |\n" ^
    "|----------------------------------------------------------|\n" ^
    "| TCPSrcPort = 80         | Output(3)                      |\n" ^
    "| IP4Src = 192.168.0.0/24 | FastFail(9999)                 |\n" ^
    "|                         | + Output(0)                    |\n" ^
    "|                         | + Enqueue(3,4)                 |\n" ^
    "|                         | + SetField(ipSrc, 192.168.0.0) |\n" ^
    "|----------------------------------------------------------|\n" ^
    "|                         | Output(0)                      |\n" ^
    "+----------------------------------------------------------+\n"

let nightmare_pattern_table = [ { 
    pattern = {  
      dlVlan = Some 500;
      dlVlanPcp = Some 30;
      dlTyp = Some 0x806;
      nwProto = Some 0x02;
      dlSrc = Some 0xdeadbeefL;
      dlDst = Some 0xbeefdeadL;
      nwSrc = Some (0xc0a80000l, 24l);
      nwDst = Some (0xc0a90000l, 18l);
      tpSrc = Some 8080;
      tpDst = Some 346356;
      inPort = Some 24l;
    };
    action = [[[ drop_action ]]];
    cookie = 0L;
    idle_timeout = Permanent;
    hard_timeout = Permanent
  } ]

let%test "string_of_flowTable prints patterns very nicely" =
  (string_of_flowTable nightmare_pattern_table) = 
    "+----------------------------------------+\n" ^
    "|  | Pattern                 | Action    |\n" ^
    "|----------------------------------------|\n" ^
    "| InPort = 24                | Output(0) |\n" ^
    "| TCPDstPort = 346356        |           |\n" ^
    "| TCPSrcPort = 8080          |           |\n" ^
    "| ipProto = 0x2 (igmp)       |           |\n" ^
    "| IP4Dst = 192.169.0.0/18    |           |\n" ^
    "| IP4Src = 192.168.0.0/24    |           |\n" ^
    "| VlanPcp = 30               |           |\n" ^
    "| Vlan = 500                 |           |\n" ^
    "| EthType = 0x806 (arp)      |           |\n" ^
    "| EthDst = 00:00:be:ef:de:ad |           |\n" ^
    "| EthSrc = 00:00:de:ad:be:ef |           |\n" ^
    "+----------------------------------------+\n"

let%test "string_of_flowTable handles blank actions, blank patterns, or a combination of the two" =
  let sample_flow_table = [
    { pattern = { match_all with inPort = Some 24l; }; action = [[[]]]; cookie=0L; idle_timeout = Permanent; hard_timeout = Permanent}; 
    { pattern = match_all; action = [[[ drop_action ]]]; cookie=0L; idle_timeout = Permanent; hard_timeout = Permanent}; 
    { pattern = match_all; action = [[[]]]; cookie=0L; idle_timeout = Permanent; hard_timeout = Permanent}; 
  ] in
  (string_of_flowTable sample_flow_table) = 
    "+-------------------------+\n" ^
    "|  | Pattern  | Action    |\n" ^
    "|-------------------------|\n" ^
    "| InPort = 24 |           |\n" ^
    "|-------------------------|\n" ^
    "|             | Output(0) |\n" ^
    "|-------------------------|\n" ^
    "|             |           |\n" ^
    "+-------------------------+\n"

let%test "string_of_flowTable takes a label for the top left corner, mostly for printing the switch" =
  (string_of_flowTable ~label:"Switch 9872345" sample_flow_table) = 
    "+-----------------------------------------------------------+\n" ^
    "| Switch 9872345 | Pattern | Action                         |\n" ^
    "|-----------------------------------------------------------|\n" ^
    "| TCPSrcPort = 80          | Output(3)                      |\n" ^
    "| IP4Src = 192.168.0.0/24  | FastFail(9999)                 |\n" ^
    "|                          | + Output(0)                    |\n" ^
    "|                          | + Enqueue(3,4)                 |\n" ^
    "|                          | + SetField(ipSrc, 192.168.0.0) |\n" ^
    "|-----------------------------------------------------------|\n" ^
    "|                          | Output(0)                      |\n" ^
    "+-----------------------------------------------------------+\n"
