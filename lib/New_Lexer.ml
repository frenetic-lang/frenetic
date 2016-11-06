type token = [%import: Frenetic_NetKAT_Tokens.token]
   [@@deriving show]

(* use custom lexbuffer to keep track of source location *)
module Sedlexing = LexBuffer
open LexBuffer

(** Signals a lexing error at the provided source location.  *)
exception LexError of (Lexing.position * string)
exception ParseError of (token * Lexing.position * Lexing.position)
let failwith buf s = raise (LexError (buf.pos, s))

let illegal buf c =
  let s = Printf.sprintf "unexpected character in NetKAT expression: '%c'" c in
  failwith buf s


(** regular expressions  *)
let letter = [%sedlex.regexp? 'A'..'Z' | 'a'..'z']
let digit = [%sedlex.regexp? '0'..'9']
let id_init = [%sedlex.regexp? letter  | '_']
let id_cont = [%sedlex.regexp? id_init | Chars ".\'" | digit ]
let id = [%sedlex.regexp? id_init, Star id_cont ]
let metaid = [%sedlex.regexp? 'A'..'Z', Star id_cont ]
let hex = [%sedlex.regexp? digit | 'a'..'f' | 'A'..'F' ]
let hexnum = [%sedlex.regexp? '0', 'x', Plus hex ]
let decnum = [%sedlex.regexp? Plus digit]
let decbyte = [%sedlex.regexp? (digit,digit,digit) | (digit,digit) | digit ]
let hexbyte = [%sedlex.regexp? hex,hex ]
let blank = [%sedlex.regexp? ' ' | '\t' ]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n" ]

(* SJS: ensure this gives longest match *)
let rec token ~ppx buf =
  match%sedlex buf with
  | eof -> EOF
  | newline -> token ~ppx buf
  | Plus blank -> token ~ppx buf
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
  | '$', id ->
    if ppx then
      ANTIQ (ascii ~skip:1 buf)
    else
      illegal buf '$'
  (* comments *)
  | "(*" ->
    comment 1 buf;
    token ~ppx buf
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
  (* meta fields *)
  | "let" -> LET
  | "var" -> VAR
  | "in" -> IN
  | metaid -> METAID (ascii buf)
  | _ -> illegal buf (Char.chr (next buf))

(* allow nested comments, like OCaml *)
and comment depth buf =
  if depth = 0 then () else
  match%sedlex buf with
  | eof -> failwith buf "Unterminated comment at EOF" 
  | "(*" -> comment (depth + 1) buf
  | "*)" -> comment (depth - 1) buf
  | any -> comment depth buf
  | _ -> assert false

let loc_token ~ppx buf =
  let start = buf.pos in
  let t = token ~ppx buf in
  let end_pos = buf.pos in
  (t, start, end_pos)

(* menhir interface *)
let parse ?(ppx=false) p buf =
  let last_token = ref Lexing.(EOF, dummy_pos, dummy_pos) in
  let next_token () = last_token := loc_token ~ppx buf; !last_token in
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | LexError (pos, s) -> raise (LexError (pos, s))
  | _ -> raise (ParseError (!last_token))

let parse_string ?pos ?ppx s p =
  parse ?ppx p (LexBuffer.of_ascii_string ?pos s)

let parse_file ?ppx ~file p =
  parse ?ppx p (LexBuffer.of_ascii_file file)
