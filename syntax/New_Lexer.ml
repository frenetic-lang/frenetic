(* Adapted from https://github.com/jaked/ambassadortothecomputers.blogspot.com/blob/4d1bde223b1788ba52cc0f74b256760d9c059ac4/_code/camlp4-custom-lexers/jq_lexer.ml
   This module is shared by Frenetic_NetKAT_Parser and Frenetic_Syntax_Extension_Parser
*)

open Core.Std
open Frenetic_NetKAT_PPX_Parser

let ppx = ref true

(* use custom lexbuffer to keep track of source location *)
module Sedlexing = LexBuffer
open LexBuffer

(** Signals a lexing error at the provided source location.  *)
exception LexError of (Lexing.position * string)
let failwith buf s = raise (LexError (buf.pos, s))

let illegal buf c =
  let s = Printf.sprintf "Illegal character in NetKAT expression: '%c'" c in
  failwith buf s


(** regular expressions  *)
let alpha = [%sedlex.regexp? 'A'..'Z' | 'a'..'z']
let num = [%sedlex.regexp? '0'..'9']
let alpha_num = [%sedlex.regexp? alpha | num ]
let id_init = [%sedlex.regexp? alpha  | '_']
let id_cont = [%sedlex.regexp? id_init | Chars ".\'" | num ]
let ident = [%sedlex.regexp? id_init, Star id_cont ]
let hex = [%sedlex.regexp? num | 'a'..'f' | 'A'..'F' ]
let hexnum = [%sedlex.regexp? '0', 'x', Plus hex ]
let decnum = [%sedlex.regexp? Plus num]
let decbyte = [%sedlex.regexp? (num,num,num) | (num,num) | num ]
let hexbyte = [%sedlex.regexp? hex,hex ]
let blank = [%sedlex.regexp? ' ' | '\t' ]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n" ]

(* SJS: ensure this gives longest match *)
let rec token buf =
  match%sedlex buf with
  | eof -> EOF
  | newline -> token buf
  | Plus blank -> token buf
  (* values *)
  | decbyte,'.',decbyte,'.',decbyte,'.',decbyte -> 
    IP4ADDR (ascii buf)
  | hexbyte,':',hexbyte,':',hexbyte,':',hexbyte,':',hexbyte,':',hexbyte ->
    MAC (ascii buf)
  | (hexnum | decnum)      -> INT (ascii buf)
  | (hexnum | decnum), 'l' -> INT (ascii buf)
  | (hexnum | decnum), 'L' -> INT (ascii buf)
  | "pipe" -> PIPE
  | "query" -> QUERY
  | '"', Star (Compl '"'), '"' -> STRING (ascii ~skip:1 ~drop:1 buf)
  (* antiquotations *)
  | '$', ident ->
    if !ppx then
      ANTIQ (ascii ~skip:1 buf)
    else
      illegal buf '$'
  (* comments *)
  | "(*" ->
    comment 1 buf;
    token buf
  (* predicates *)
  | "true" -> TRUE
  | "false" -> FALSE
  | "and" -> AND
  | "or" -> OR
  | "!" -> NOT
  | '=' -> EQUALS
  (* policies *)
  | "id" -> ID
  | "drop" -> DROP
  | "filter" -> FILTER
  | ":=" -> ASSIGN
  | ';' -> SEMICOLON
  (* SJS: let's start using `+` instead of `|` ? *)
  | '+' -> PLUS
  | '*' -> STAR
  | "=>" -> LINK
  | "@" -> AT
  | '/' -> SLASH
  (* fields *)
  | "switch" -> SWITCH
  | "port" -> PORT
  | "ethSrc" -> ETHSRC
  | "ethDst" -> ETHDST
  | "vlan" -> VLAN
  | "vlanId" -> VLANPCP
  | "ethTyp" -> ETHTYPE
  | "ipProto" -> IPPROTO
  | "ip4Src" -> IP4SRC
  | "ip4Dst" -> IP4DST
  | "tcpSrcPort" -> TCPSRCPORT
  | "tcpDstPort" -> TCPDSTPORT
  (* syntax sugar *)
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "while" -> WHILE
  | "do" -> DO
  (* parenths *)
  | '(' -> LPAR
  | ')' -> RPAR
  | "begin" -> BEGIN
  | "end" -> END
  (* SJS / TODO: meta fields *)
  | _ -> illegal buf (Char.of_int_exn (next buf))

(* allow nested comments, like OCaml *)
and comment depth buf =
  if depth = 0 then () else
  match%sedlex buf with
  | eof -> failwith buf "Unterminated comment at EOF" 
  | "(*" -> comment (depth + 1) buf
  | "*)" -> comment (depth - 1) buf
  | any -> comment depth buf
  | _ -> assert false

let loc_token buf =
  let start = buf.pos in
  let t = token buf in
  let end_pos = buf.pos in
  (t, start, end_pos)
