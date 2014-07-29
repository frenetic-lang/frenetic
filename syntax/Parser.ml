open Camlp4.PreCast
open MyLexer
module AQ = Syntax.AntiquotSyntax

module Gram = MakeGram(MyLexer)

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
    | "!"; a = nk_pred_atom -> <:expr<NetKAT_Types.Neg $a$>>
    | "switch"; "="; sw = nk_int64 ->
        <:expr<NetKAT_Types.(Test (Switch $sw$))>>
    | "port"; "="; n = nk_int32 ->
        <:expr<NetKAT_Types.Test (NetKAT_Types.(Location (Physical $n$)))>>
    | "vlan"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (Vlan $n$))>>
    | "vlanPcp"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (VlanPcp $n$))>>
    | "ethType"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (EthType $n$))>>
    | "ipProto"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (IPProto $n$))>>
    | "tcpSrcPort"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (TCPSrcPort $n$))>>
    | "tcpDstPort"; "="; n = nk_int ->
        <:expr<NetKAT_Types.(Test (TCPDstPort $n$))>>
    | "ethSrc"; ":="; n = nk_int64 ->
        <:expr<NetKAT_Types.(Test (EthSrc $n$))>>
    | "ethDst"; ":="; n = nk_int64 ->
        <:expr<NetKAT_Types.(Test (EthDst $n$))>>
    | "ip4Src"; "="; n = nk_ipv4 ->
        <:expr<NetKAT_Types.(Test (IP4Src ($n$,32l)))>>
    | "ip4Dst"; "="; n = nk_ipv4 ->
        <:expr<NetKAT_Types.(Test (IP4Dst ($n$, 32l)))>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_pred_and : [[
      a = nk_pred_atom -> <:expr<$a$>>
    | a = nk_pred_and; "&&"; b = nk_pred_atom ->
      <:expr<NetKAT_Types.And ($a$, $b$)>>
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> <:expr<$a$>>
    | a = nk_pred_or; "||"; b = nk_pred_and ->
      <:expr<NetKAT_Types.Or ($a$, $b$)>>
  ]];

  nk_pred: [[
      a = nk_pred_or -> <:expr<$a$>>
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" -> <:expr<$p$>>
    | "id" -> <:expr<NetKAT_Types.Filter NetKAT_Types.True>>
    | "drop" -> <:expr<NetKAT_Types.Filter NetKAT_Types.False>>
    | "filter"; a = nk_pred -> <:expr<NetKAT_Types.Filter $a$>>
    | "switch"; ":="; sw = nk_int64 ->
        <:expr<NetKAT_Types.(Mod (Switch $sw$))>>
    | "port"; ":="; n = nk_int32 ->
        <:expr<NetKAT_Types.(Mod (Location (Physical $n$)))>>
    | "vlan"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (Vlan $n$))>>
    | "vlanPcp"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (VlanPcp $n$))>>
    | "ethType"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (EthType $n$))>>
    | "ipProto"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (IPProto $n$))>>
    | "tcpSrcPort"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (TCPSrcPort $n$))>>
    | "tcpDstPort"; ":="; n = nk_int ->
        <:expr<NetKAT_Types.(Mod (TCPDstPort $n$))>>
    | "ethSrc"; ":="; n = nk_int64 ->
        <:expr<NetKAT_Types.(Mod (EthSrc $n$))>>
    | "ethDst"; ":="; n = nk_int64 ->
        <:expr<NetKAT_Types.(Mod (EthDst $n$))>>
    | "ip4Src"; "="; n = nk_ipv4 ->
        <:expr<NetKAT_Types.(Mod (IP4Src $n$))>>
    | "ip4Dst"; "="; n = nk_ipv4 ->
        <:expr<NetKAT_Types.(Mod (IP4Dst $n$))>>
    | `ANTIQUOT s -> AQ.parse_expr _loc s
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> <:expr<$p$>>
    | p = nk_pol_star; "*" -> <:expr<NetKAT_Types.Star $p$>>
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> <:expr<$p$>>
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      <:expr<NetKAT_Types.Seq ($p$, $q$)>>
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> <:expr<$p$>>
    | p = nk_pol_union; "+"; q = nk_pol_seq ->
      <:expr<NetKAT_Types.Union ($p$, $q$)>>
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> <:expr<$p$>>
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      <:expr<NetKAT_Types.(Union(Seq(Filter $a$, $p$), Seq(Filter (Neg $a$), $q$)))>>
  ]];

  nk_pol : [[
    p = nk_pol_cond -> <:expr<$p$>>
  ]];

END
