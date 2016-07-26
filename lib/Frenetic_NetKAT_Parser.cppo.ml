#ifdef AST
  #define MK(arg) <:expr< arg >>
  #define ID(arg) $arg$
  #define STR(arg) $`str:arg$
  #define AQ | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s
#else
  #define MK(arg) arg
  #define ID(arg) arg
  #define STR(arg) arg
  #define AQ
#endif

open Core.Std
open Camlp4.PreCast
module Gram = MakeGram(Frenetic_NetKAT_Lexer)
open Frenetic_NetKAT_Lexer
open Frenetic_NetKAT

(* SJS: hack, but does the job. Reading [undef] will cause sefgault. *)
let undef : 'a = Obj.magic 0

let nk_pred_eoi = Gram.Entry.mk "nk_pred_eoi"
let nk_pred = Gram.Entry.mk "nk_pred"
let nk_pred_atom = Gram.Entry.mk "nk_pred_atom"
let nk_pred_not = Gram.Entry.mk "nk_pred_not"
let nk_pred_and = Gram.Entry.mk "nk_pred_and"
let nk_pred_or = Gram.Entry.mk "nk_pred_or"
let nk_pol_eoi = Gram.Entry.mk "nk_pol_eoi"
let nk_pol = Gram.Entry.mk "nk_pol"
let nk_pol_atom = Gram.Entry.mk "nk_pol_atom"
let nk_pol_seq = Gram.Entry.mk "nk_pol_seq"
let nk_pol_star = Gram.Entry.mk "nk_pol_star"
let nk_pol_union = Gram.Entry.mk "nk_pol_union"
let nk_pol_cond = Gram.Entry.mk "nk_pol_cond"
let nk_pol_meta = Gram.Entry.mk "nk_pol_meta"
let nk_let = Gram.Entry.mk "nk_let"
let nk_int64 = Gram.Entry.mk "nk_int64"
let nk_int32 = Gram.Entry.mk "nk_int32"
let nk_int = Gram.Entry.mk "nk_int"
let nk_ipv4 = Gram.Entry.mk "nk_ipv4"
let nk_loc = Gram.Entry.mk "nk_loc"
let nk_string_constant = Gram.Entry.mk "nk_string_constant"
let nk_pkt_dest = Gram.Entry.mk "nk_pkt_dest"

EXTEND Gram

  nk_int64: [[
      n = INT -> MK(Int64.of_string STR(n))
    | n = INT64 -> MK(Int64.of_string STR(n))
    AQ
  ]];

  nk_int32: [[
      n = INT -> MK(Int32.of_string STR(n))
    | n = INT32 -> MK(Int32.of_string STR(n))
    AQ
  ]];

  nk_int: [[
      n = INT -> MK(Int.of_string STR(n))
    AQ
  ]];

  nk_ipv4: [[
      n = IP4ADDR ->
        MK(Ipaddr.V4.(to_int32 (of_string_exn STR(n))))
     AQ
  ]];

  nk_loc: [[
      switch = nk_int64; "@"; port = nk_int64 ->
        MK((ID(switch),ID(port)))
  ]];

  nk_string_constant: [[
      sc = STRING_CONSTANT -> MK(STR(sc))
  ]];

  nk_pred_atom: [[
      "("; a = nk_pred; ")" ->
      a
    | "begin"; a = nk_pred; "end" ->
      a
    | "true" ->
      MK(True)
    | "false" ->
      MK(False)
    | "switch"; "="; sw = nk_int64 ->
      MK(Test (Switch ID(sw)))
    | "port"; "="; n = nk_int32 ->
      MK(Test (Location (Physical ID(n))))
    | "vswitch"; "="; sw = nk_int64 ->
      MK((Test (VSwitch ID(sw))))
    | "vport"; "="; n = nk_int64 ->
      MK(Test (VPort ID(n)))
    | "vfabric"; "="; vfab = nk_int64 ->
      MK(Test (VFabric ID(vfab)))
    | "vlanId"; "="; n = nk_int ->
      MK(Test (Vlan ID(n)))
    | "vlanPcp"; "="; n = nk_int ->
      MK(Test (VlanPcp ID(n)))
    | "ethTyp"; "="; n = nk_int ->
      MK(Test (EthType ID(n)))
    | "ipProto"; "="; n = nk_int ->
      MK(Test (IPProto ID(n)))
    | "tcpSrcPort"; "="; n = nk_int ->
      MK(Test (TCPSrcPort ID(n)))
    | "tcpDstPort"; "="; n = nk_int ->
      MK(Test (TCPDstPort ID(n)))
    | "ethSrc"; "="; n = nk_int64 ->
      MK(Test (EthSrc ID(n)))
    | "ethDst"; "="; n = nk_int64 ->
      MK(Test (EthDst ID(n)))
    | "ip4Src"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      MK(Test (IP4Src (ID(n), ID(m))))
    | "ip4Src"; "="; n = nk_ipv4 ->
      MK(Test (IP4Src (ID(n), 32l)))
    | "ip4Dst"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      MK(Test (IP4Dst (ID(n), ID(m))))
    | "ip4Dst"; "="; n = nk_ipv4 ->
      MK(Test (IP4Dst (ID(n), 32l)))
    | id=METAID; "="; n = nk_int64 ->
      MK(Test (Meta (STR(id), ID(n))))
    AQ
  ]];

  nk_pred_not : [[
      a = nk_pred_atom -> a
    | "not"; a = nk_pred_not ->
      MK(Neg ID(a))
  ]];

  nk_pred_and : [[
      a = nk_pred_not -> a
    | a = nk_pred_and; "and"; b = nk_pred_not ->
      MK(And (ID(a), ID(b)))
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> a
    | a = nk_pred_or; "or"; b = nk_pred_and ->
      MK(Or (ID(a), ID(b)))
  ]];

  nk_pred: [[
      a = nk_pred_or -> a
  ]];

  nk_pkt_dest: [[
      n = nk_int32 -> MK(Physical n)
    | "query"; "("; q = nk_string_constant; ")" -> MK(Query q)
    | "pipe"; "("; p = nk_string_constant; ")" -> MK(Pipe p)
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" ->
      p
    | "begin"; p = nk_pol; "end" ->
      p
    | "id" ->
      MK(id)
    | "drop" ->
      MK(drop)
    | "filter"; a = nk_pred ->
      MK(Filter ID(a))
    | "switch"; ":="; sw = nk_int64 ->
      MK(Mod (Switch ID(sw)))
    | "port"; ":="; l = nk_pkt_dest ->
      MK(Mod (Location l))
    | "vswitch"; ":="; sw = nk_int64 ->
      MK(Mod (VSwitch ID(sw)))
    | "vport"; ":="; n = nk_int64 ->
      MK(Mod (VPort ID(n)))
    | "vfabric"; ":="; vfab = nk_int64 ->
      MK(Mod (VFabric ID(vfab)))
    | "ethSrc"; ":="; n = nk_int64 ->
      MK(Mod (EthSrc ID(n)))
    | "ethDst"; ":="; n = nk_int64 ->
      MK(Mod (EthDst ID(n)))
    | "ethTyp"; ":="; n = nk_int ->
      MK(Mod (EthType ID(n)))
    | "vlanId"; ":="; n = nk_int ->
      MK(Mod (Vlan ID(n)))
    | "vlanPcp"; ":="; n = nk_int ->
      MK(Mod (VlanPcp ID(n)))
    | "ip4Src"; ":="; n = nk_ipv4 ->
      MK(Mod (IP4Src(ID(n), 32l)))
    | "ip4Dst"; ":="; n = nk_ipv4 ->
      MK(Mod (IP4Dst(ID(n), 32l)))
    | "ipProto"; ":="; n = nk_int ->
      MK(Mod (IPProto ID(n)))
    | "tcpSrcPort"; ":="; n = nk_int ->
      MK(Mod (TCPSrcPort ID(n)))
    | "tcpDstPort"; ":="; n = nk_int ->
      MK(Mod (TCPDstPort ID(n)))
    | id=METAID; ":="; n = nk_int64 ->
      MK(Mod (Meta (STR(id), ID(n))))
    | loc1 = nk_loc; "=>"; loc2 = nk_loc -> MK(
      let (sw1, pt1) = ID(loc1) in
      let (sw2, pt2) = ID(loc2) in
      let pt1 = Int64.to_int32_exn pt1 in
      let pt2 = Int64.to_int32_exn pt2 in
      Link (sw1, pt1, sw2, pt2))
    | loc1 = nk_loc; "=>>"; loc2 = nk_loc -> MK(
      let (sw1, pt1) = ID(loc1) in
      let (sw2, pt2) = ID(loc2) in
      MK(VLink (sw1, pt1, sw2, pt2)))
    AQ
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> p
    | p = nk_pol_star; "*" ->
      MK(Star ID(p))
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> p
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      MK(Seq (ID(p), ID(q)))
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> p
    | p = nk_pol_union; "|"; q = nk_pol_seq ->
      MK(Union (ID(p), ID(q)))
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> p
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      MK(Union(Seq(Filter ID(a), ID(p)), Seq(Filter (Neg ID(a)), ID(q))))
  ]];

  nk_let : [[
      "let" -> MK(false)
    | "var" -> MK(true)
  ]];

  nk_pol_meta : [[
      p = nk_pol_cond -> p
    | mut=nk_let; id=METAID; ":="; v=nk_int64; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Const ID(v), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "switch" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (Switch undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "port" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (Location undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ethSrc" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (EthSrc undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ethDst" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (EthDst undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "vlanId" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (Vlan undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "vlanPcp" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (VlanPcp undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ethTyp" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (EthType undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ipProto" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (IPProto undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ip4Src" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (IP4Src (Obj.magic 0, Obj.magic 0)), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "ip4Dst" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (IP4Dst (Obj.magic 0, Obj.magic 0)), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "tcpSrcPort" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (TCPSrcPort undef), ID(mut), ID(p)))
    | mut=nk_let; id=METAID; ":="; "tcpDstPort" ; "in"; p = nk_pol_meta ->
      MK(Let (STR(id), Alias (TCPDstPort undef), ID(mut), ID(p)))
  ]];

  nk_pol : [[ p = nk_pol_meta -> p ]];

  nk_pol_eoi: [[ x = nk_pol; `EOI -> x ]];
  nk_pred_eoi: [[ x = nk_pred; `EOI -> x ]];

END


let report loc e =
  failwith (Loc.to_string loc ^ ": " ^ Exn.to_string e)

let policy_of_stream ?(loc=(Loc.mk "<N/A>")) (s : char Stream.t) =
  try Gram.parse nk_pol_eoi loc s
  with Loc.Exc_located (loc, e) -> report loc e

let pred_of_stream ?(loc=(Loc.mk "<N/A>")) (s : char Stream.t) =
  try Gram.parse nk_pred_eoi loc s
  with Loc.Exc_located (loc, e) -> report loc e

let policy_of_string ?(loc=(Loc.mk "<N/A>")) (s : string) =
  policy_of_stream ~loc (Stream.of_string s)

let pred_of_string ?(loc=(Loc.mk "<N/A>")) (s : string) =
  pred_of_stream ~loc (Stream.of_string s)

let policy_of_file (file : string) =
  In_channel.with_file file ~f:(fun ch -> policy_of_stream ~loc:(Loc.mk file) (Stream.of_channel ch))

let pred_of_file (file : string) =
  In_channel.with_file file ~f:(fun ch -> pred_of_stream ~loc:(Loc.mk file) (Stream.of_channel ch))
