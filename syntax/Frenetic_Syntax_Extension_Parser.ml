open Camlp4.PreCast
open Frenetic_NetKAT_Lexer
module AQ = Syntax.AntiquotSyntax

module Gram = MakeGram(Frenetic_NetKAT_Lexer)

let nk_pred = Gram.Entry.mk "nk_pred"
let nk_pred_atom = Gram.Entry.mk "nk_pred_atom"
let nk_pred_and = Gram.Entry.mk "nk_pred_and"
let nk_pred_or = Gram.Entry.mk "nk_pred_or"
let nk_pol = Gram.Entry.mk "nk_pol"
let nk_pol_atom = Gram.Entry.mk "nk_pol_atom"
let nk_pol_seq = Gram.Entry.mk "nk_pol_seq"
let nk_pol_star = Gram.Entry.mk "nk_pol_star"
let nk_pol_union = Gram.Entry.mk "nk_pol_union"
let nk_pol_cond = Gram.Entry.mk "nk_pol_cond"
let nk_int64 = Gram.Entry.mk "nk_int64"
let nk_int32 = Gram.Entry.mk "nk_int32"
let nk_int = Gram.Entry.mk "nk_int"
let nk_ipv4 = Gram.Entry.mk "nk_ipv4"

EXTEND Gram

  nk_int64: [[
      n = INT -> <:expr<Core.Std.Int64.of_int_exn (int_of_string $`str:n$)>>
    | n = INT64 -> <:expr<Core.Std.Int64.of_string $`str:n$>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_int32: [[
      n = INT -> <:expr<Core.Std.Int32.of_int_exn (int_of_string $`str:n$)>>
    | n = INT32 -> <:expr<Core.Std.Int32.of_string $`str:n$>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_int: [[
       n = INT -> <:expr<int_of_string $`str:n$>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_ipv4: [[
        n = IP4ADDR ->
        let ip = Ipaddr.V4.(to_int32 (of_string_exn n)) in
        <:expr<$`int32:ip$>>
      | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_pred_atom: [[
      "("; a = nk_pred; ")" -> <:expr<$a$>>
    | "true" -> <:expr<NetKAT_Misc.pred_true>>
    | "false" -> <:expr<NetKAT_Misc.pred_false>>
    | "!"; a = nk_pred_atom -> <:expr<Frenetic_NetKAT.Neg $a$>>
    | "switch"; "="; sw = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Test (Switch $sw$))>>
    | "port"; "="; n = nk_int32 ->
        <:expr<Frenetic_NetKAT.Test (Frenetic_NetKAT.(Location (Physical $n$)))>>
    | "vlan"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (Vlan $n$))>>
    | "vlanPcp"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (VlanPcp $n$))>>
    | "ethType"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (EthType $n$))>>
    | "ipProto"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (IPProto $n$))>>
    | "tcpSrcPort"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (TCPSrcPort $n$))>>
    | "tcpDstPort"; "="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Test (TCPDstPort $n$))>>
    | "ethSrc"; "="; n = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Test (EthSrc $n$))>>
    | "ethDst"; "="; n = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Test (EthDst $n$))>>
    | "ip4Src"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
        <:expr<Frenetic_NetKAT.(Test (IP4Src ($n$, $m$)))>>
    | "ip4Src"; "="; n = nk_ipv4 ->
        <:expr<Frenetic_NetKAT.(Test (IP4Src ($n$, 32l)))>>
    | "ip4Dst"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
        <:expr<Frenetic_NetKAT.(Test (IP4Dst ($n$, $m$)))>>
    | "ip4Dst"; "="; n = nk_ipv4 ->
        <:expr<Frenetic_NetKAT.(Test (IP4Dst ($n$, 32l)))>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_pred_and : [[
      a = nk_pred_atom -> <:expr<$a$>>
    | a = nk_pred_and; "&&"; b = nk_pred_atom ->
      <:expr<Frenetic_NetKAT.And ($a$, $b$)>>
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> <:expr<$a$>>
    | a = nk_pred_or; "||"; b = nk_pred_and ->
      <:expr<Frenetic_NetKAT.Or ($a$, $b$)>>
  ]];

  nk_pred: [[
      a = nk_pred_or -> <:expr<$a$>>
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" -> <:expr<$p$>>
    | "id" -> <:expr<Frenetic_NetKAT.Filter Frenetic_NetKAT.True>>
    | "drop" -> <:expr<Frenetic_NetKAT.Filter Frenetic_NetKAT.False>>
    | "filter"; a = nk_pred -> <:expr<Frenetic_NetKAT.Filter $a$>>
    | "switch"; ":="; sw = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Mod (Switch $sw$))>>
    | "port"; ":="; n = nk_int32 ->
        <:expr<Frenetic_NetKAT.(Mod (Location (Physical $n$)))>>
    | "ethSrc"; ":="; n = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Mod (EthSrc $n$))>>
    | "ethDst"; ":="; n = nk_int64 ->
        <:expr<Frenetic_NetKAT.(Mod (EthDst $n$))>>
    | "ethType"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (EthType $n$))>>
    | "vlan"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (Vlan $n$))>>
    | "vlanPcp"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (VlanPcp $n$))>>
    | "ip4Src"; ":="; n = nk_ipv4 ->
        <:expr<Frenetic_NetKAT.(Mod (IP4Src($n$,32l)))>>
    | "ip4Dst"; ":="; n = nk_ipv4 ->
        <:expr<Frenetic_NetKAT.(Mod (IP4Dst($n$,32l)))>>
    | "ipProto"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (IPProto $n$))>>
    | "tcpSrcPort"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (TCPSrcPort $n$))>>
    | "tcpDstPort"; ":="; n = nk_int ->
        <:expr<Frenetic_NetKAT.(Mod (TCPDstPort $n$))>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> <:expr<$p$>>
    | p = nk_pol_star; "*" -> <:expr<Frenetic_NetKAT.Star $p$>>
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> <:expr<$p$>>
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      <:expr<Frenetic_NetKAT.Seq ($p$, $q$)>>
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> <:expr<$p$>>
    | p = nk_pol_union; "+"; q = nk_pol_seq ->
      <:expr<Frenetic_NetKAT.Union ($p$, $q$)>>
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> <:expr<$p$>>
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      <:expr<Frenetic_NetKAT.(Union(Seq(Filter $a$, $p$), Seq(Filter (Neg $a$), $q$)))>>
  ]];

  nk_pol : [[
    p = nk_pol_cond -> <:expr<$p$>>
  ]];

END
