(* TODO: add vswitch and vport *)

(* Simple parser for NetKAT ... no antiquoting, etc.
   This parser has a common structure with Frenetic_Syntax_Extension_Parser, but outputs
   pure NetKAT data structures instead of OCaml AST's.  It's be nice if you could share the logic
   but that doesn't look possible because.

   So IF YOU CHANGE THE GRAMMAR HERE, be sure to change it in Frenetic_Syntax_Extension_Parser as well
)*)

open Core.Std
open Camlp4.PreCast
open Frenetic_NetKAT_Lexer
module Gram = MakeGram(Frenetic_NetKAT_Lexer)

let nk_pred = Gram.Entry.mk "nk_pred"
let nk_pred_atom = Gram.Entry.mk "nk_pred_atom"
let nk_pred_not = Gram.Entry.mk "nk_pred_not"
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
      n = INT -> Core.Std.Int64.of_int_exn (int_of_string n)
    | n = INT64 -> Core.Std.Int64.of_string n
    (* Not sure how to handle this yet, if we even need to
    | `ANTIQUOT s -> illegal "$ syntax is not allowed in the simple parser"
  *)
  ]];

  nk_int32: [[
      n = INT -> Core.Std.Int32.of_int_exn (int_of_string n)
    | n = INT32 -> Core.Std.Int32.of_string n
  ]];

  nk_int: [[
       n = INT -> int_of_string n
  ]];

  nk_ipv4: [[
        n = IP4ADDR ->
        let ip = Ipaddr.V4.(to_int32 (of_string_exn n)) in
        ip
  ]];

  nk_pred_atom: [[
      "("; a = nk_pred; ")" -> a
    | "true" -> 
      Frenetic_NetKAT.True
    | "false" -> 
      Frenetic_NetKAT.False
    | "switch"; "="; sw = nk_int64 -> 
      Frenetic_NetKAT.(Test (Switch sw))
    | "port"; "="; n = nk_int32 -> 
      Frenetic_NetKAT.Test (Frenetic_NetKAT.(Location (Physical n)))
    | "vswitch"; "="; sw = nk_int64 -> 
      Frenetic_NetKAT.(Test (VSwitch sw))
    | "vport"; "="; n = nk_int64 -> 
      Frenetic_NetKAT.Test (Frenetic_NetKAT.(VPort n))
    | "vlanId"; "="; n = nk_int -> 
      Frenetic_NetKAT.(Test (Vlan n))
    | "vlanPcp"; "="; n = nk_int -> 
      Frenetic_NetKAT.(Test (VlanPcp n))
    | "ethTyp"; "="; n = nk_int ->
      Frenetic_NetKAT.(Test (EthType n))
    | "ipProto"; "="; n = nk_int ->
      Frenetic_NetKAT.(Test (IPProto n))
    | "tcpSrcPort"; "="; n = nk_int ->
      Frenetic_NetKAT.(Test (TCPSrcPort n))
    | "tcpDstPort"; "="; n = nk_int ->
      Frenetic_NetKAT.(Test (TCPDstPort n))
    | "ethSrc"; "="; n = nk_int64 ->
      Frenetic_NetKAT.(Test (EthSrc n))
    | "ethDst"; "="; n = nk_int64 ->
      Frenetic_NetKAT.(Test (EthDst n))
    | "ip4Src"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      Frenetic_NetKAT.(Test (IP4Src (n, m)))
    | "ip4Src"; "="; n = nk_ipv4 ->
      Frenetic_NetKAT.(Test (IP4Src (n, 32l)))
    | "ip4Dst"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      Frenetic_NetKAT.(Test (IP4Dst (n, m)))
    | "ip4Dst"; "="; n = nk_ipv4 ->
      Frenetic_NetKAT.(Test (IP4Dst (n, 32l)))
  ]];

  nk_pred_not : [[
      a = nk_pred_atom -> a 
    | "not"; a = nk_pred_not -> 
      Frenetic_NetKAT.Neg a
  ]];

  nk_pred_and : [[
      a = nk_pred_not -> a
    | a = nk_pred_and; "and"; b = nk_pred_not ->
      Frenetic_NetKAT.And (a, b)
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> a
    | a = nk_pred_or; "or"; b = nk_pred_and ->
      Frenetic_NetKAT.Or (a, b)
  ]];

  nk_pred: [[
      a = nk_pred_or -> a
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" -> 
      p
    | "id" -> 
      Frenetic_NetKAT.Filter Frenetic_NetKAT.True
    | "drop" -> 
      Frenetic_NetKAT.Filter Frenetic_NetKAT.False
    | "filter"; a = nk_pred -> 
      Frenetic_NetKAT.Filter a
    | "switch"; ":="; sw = nk_int64 ->
      Frenetic_NetKAT.(Mod (Switch sw))
    | "port"; ":="; n = nk_int32 ->
      Frenetic_NetKAT.(Mod (Location (Physical n)))
    | "vswitch"; ":="; sw = nk_int64 ->
      Frenetic_NetKAT.(Mod (VSwitch sw))
    | "vport"; ":="; n = nk_int64 ->
      Frenetic_NetKAT.(Mod (VPort n))
    | "ethSrc"; ":="; n = nk_int64 ->
      Frenetic_NetKAT.(Mod (EthSrc n))
    | "ethDst"; ":="; n = nk_int64 ->
      Frenetic_NetKAT.(Mod (EthDst n))
    | "ethTyp"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (EthType n))
    | "vlanId"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (Vlan n))
    | "vlanPcp"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (VlanPcp n))
    | "ip4Src"; ":="; n = nk_ipv4 ->
      Frenetic_NetKAT.(Mod (IP4Src(n,32l)))
    | "ip4Dst"; ":="; n = nk_ipv4 ->
      Frenetic_NetKAT.(Mod (IP4Dst(n,32l)))
    | "ipProto"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (IPProto n))
    | "tcpSrcPort"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (TCPSrcPort n))
    | "tcpDstPort"; ":="; n = nk_int ->
      Frenetic_NetKAT.(Mod (TCPDstPort n))
    | switch1 = nk_int64; "@"; port1 = nk_int32; "=>"; switch2 = nk_int64; "@"; port2 = nk_int32 ->
      Frenetic_NetKAT.(Link (switch1, port1, switch2, port2))
    | switch1 = nk_int64; "@"; port1 = nk_int64; "=>>"; switch2 = nk_int64; "@"; port2 = nk_int64 ->
      Frenetic_NetKAT.(VLink (switch1, port1, switch2, port2))
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> 
      p
    | p = nk_pol_star; "*" -> 
      Frenetic_NetKAT.Star p
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> 
      p
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      Frenetic_NetKAT.Seq (p, q)
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> 
      p
    | p = nk_pol_union; "|"; q = nk_pol_seq ->
      Frenetic_NetKAT.Union (p, q)
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> 
      p
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      Frenetic_NetKAT.(Union(Seq(Filter a, p), Seq(Filter (Neg a), q)))
  ]];

  nk_pol : [[
    p = nk_pol_cond -> 
    p
  ]];

END

let policy_from_string s =
  Gram.parse_string nk_pol (Loc.mk "<string>") s

let pred_from_string s = 
  Gram.parse_string nk_pred (Loc.mk "<string>") s
