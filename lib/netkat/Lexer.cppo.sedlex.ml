module Location = struct
  include Location
  let pp = print
end

(* FIXME: while ppx_import is not compatible with jbuilder, simply copy and paste
   token type here as a workaround. *)
(* type token = [@import Tokens.token] [@@deriving show] *)
#include "Tokens.ml"
[@@deriving show]

(* use custom lexbuffer to keep track of source location *)
module Sedlexing = LexBuffer
open LexBuffer

(** Signals a lexing error at the provided source location.  *)
exception LexError of (Lexing.position * string)

(** Signals a parsing error at the provided token and its start and end locations. *)
exception ParseError of (token * Lexing.position * Lexing.position)

(** Register exceptions for pretty printing *)
let _ =
  let open Location in
  register_error_of_exn (function
    | LexError (pos, msg) ->
      let loc = { loc_start = pos; loc_end = pos; loc_ghost = false} in
      Some { loc; msg; sub=[]; if_highlight=""; }
    | ParseError (token, loc_start, loc_end) ->
      let loc = Location.{ loc_start; loc_end; loc_ghost = false} in
      let msg =
        show_token token
        |> Printf.sprintf "parse error while reading token '%s'" in
      Some { loc; msg; sub=[]; if_highlight=""; }
    | _ -> None)

let failwith buf s = raise (LexError (buf.pos, s))

let illegal buf c =
  Char.escaped c
  |> Printf.sprintf "unexpected character in NetKAT expression: '%s'"
  |> failwith buf

(** regular expressions  *)
let letter = [%sedlex.regexp? 'A'..'Z' | 'a'..'z']
let digit = [%sedlex.regexp? '0'..'9']
let id_init = [%sedlex.regexp? letter  | '_']
let id_cont = [%sedlex.regexp? id_init | Chars ".\'" | digit ]
let id = [%sedlex.regexp? id_init, Star id_cont ]
let hex = [%sedlex.regexp? digit | 'a'..'f' | 'A'..'F' ]
let hexnum = [%sedlex.regexp? '0', 'x', Plus hex ]
let decnum = [%sedlex.regexp? Plus digit]
let decbyte = [%sedlex.regexp? (digit,digit,digit) | (digit,digit) | digit ]
let hexbyte = [%sedlex.regexp? hex,hex ]
let blank = [%sedlex.regexp? ' ' | '\t' ]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n" ]

(** swallows whitespace and comments *)
let rec garbage buf =
  match%sedlex buf with
  | newline -> garbage buf
  | Plus blank -> garbage buf
  | "(*" -> comment 1 buf
  | _ -> ()

(* allow nested comments, like OCaml *)
and comment depth buf =
  if depth = 0 then garbage buf else
  match%sedlex buf with
  | eof -> failwith buf "Unterminated comment at EOF"
  | "(*" -> comment (depth + 1) buf
  | "*)" -> comment (depth - 1) buf
  | any -> comment depth buf
  | _ -> assert false

(** anitquotation brackets  *)
let antiq ~loc_start buf =
  match%sedlex buf with
  | Star (Compl '}') ->
    let code = ascii buf in
    begin match%sedlex buf with
    | '}' -> ()
    | _ -> failwith buf "unterminated anitquotation brace"
    end;
    let loc_end = next_loc buf in
    let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
    ANTIQ (code, loc)
  | _ -> assert false

(** iverson brackets  *)
let iverson ~loc_start buf =
  match%sedlex buf with
  | Star (Compl ']') ->
    let code = ascii buf in
    begin match%sedlex buf with
    | ']' -> ()
    | _ -> failwith buf "unterminated iverson bracket"
    end;
    let loc_end = next_loc buf in
    let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
    IVERSON (code, loc)
  | _ -> assert false

(** returns the next token *)
let token ~ppx ~loc_start buf =
  garbage buf;
  match%sedlex buf with
  | eof -> EOF
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
  | "dup" -> DUP
  (* antiquotations *)
  | '{' ->
    if not ppx then
      illegal buf '{'
    else
      antiq ~loc_start buf
  | '[' ->
    if not ppx then
      illegal buf '['
    else
      iverson ~loc_start buf
  (* predicates *)
  | "true" -> TRUE
  | "false" -> FALSE
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | '=' -> EQUALS
  (* policies *)
  | "id" -> ID
  | "drop" -> DROP
  | "filter" -> FILTER
  | ":=" -> ASSIGN
  | ';' -> SEMICOLON
  | '+' -> PLUS
  | '*' -> STAR
  | "=>" -> LINK
  | "=>>" -> VLINK
  | "@" -> AT
  | '/' -> SLASH
  (* fields *)
  | "switch" -> SWITCH
  | "port" -> PORT
  | "vswitch" -> VSWITCH
  | "vport" -> VPORT
  | "vfabric" -> VFABRIC
  | "ethSrc" -> ETHSRC
  | "ethDst" -> ETHDST
  | "vlanId" -> VLAN
  | "vlanPcp" -> VLANPCP
  | "ethTyp" -> ETHTYPE
  | "ipProto" -> IPPROTO
  | "ip4Src" -> IP4SRC
  | "ip4Dst" -> IP4DST
  | "tcpSrcPort" -> TCPSRCPORT
  | "tcpDstPort" -> TCPDSTPORT
  (* portless *)
  | "from" -> FROM
  | "loc" -> ABSTRACTLOC
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
  | '`', id -> METAID (ascii buf ~skip:1)
  | _ -> illegal buf (Char.chr (next buf))

(** wrapper around `token` that records start and end locations *)
let loc_token ~ppx buf =
  let () = garbage buf in (* dispose of garbage before recording start location *)
  let loc_start = next_loc buf in
  let t = token ~ppx ~loc_start buf in
  let loc_end = next_loc buf in
  (t, loc_start, loc_end)


(** menhir interface *)
type ('token, 'a) parser = ('token, 'a) MenhirLib.Convert.traditional

let parse ?(ppx=false) buf p =
  let last_token = ref Lexing.(EOF, dummy_pos, dummy_pos) in
  let next_token () = last_token := loc_token ~ppx buf; !last_token in
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | e ->
    begin match e with
    | LexError _ | Syntaxerr.Error _ -> raise e
    | _ -> raise (ParseError (!last_token))
    end

let parse_string ?ppx ?pos s p =
  parse ?ppx (LexBuffer.of_ascii_string ?pos s) p

let parse_file ?ppx ~file p =
  parse ?ppx (LexBuffer.of_ascii_file file) p
