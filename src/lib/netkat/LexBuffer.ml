(** A custom lexbuffer that automatically keeps track of the source location.
    This module is a thin wrapper arounds sedlexing's default buffer, which does
    not provide this functionality. *)

open Core

(** the lex buffer type *)
type t = {
  buf : Sedlexing.lexbuf;
  mutable pos : Lexing.position;
  mutable pos_mark : Lexing.position;
  mutable last_char : int option;
  mutable last_char_mark : int option;
}

let of_sedlex ?(file="<n/a>") ?pos buf =
  let pos = Option.value pos ~default:Lexing.{
    pos_fname = file;
    pos_lnum = 1; (* line number *)
    pos_bol = 0; (* offset of beginning of current line *)
    pos_cnum = 0; (* total offset *) }
  in
  {  buf; pos; pos_mark = pos; last_char = None; last_char_mark = None; }

let of_ascii_string ?pos s =
  of_sedlex ?pos Sedlexing.(Latin1.from_string s) 

let of_ascii_file file =
  let chan = In_channel.create file in
  of_sedlex ~file Sedlexing.(Latin1.from_channel chan)

(** The next four functions are used by sedlex internally.
    See https://www.lexifi.com/sedlex/libdoc/Sedlexing.html.  *)
let mark lexbuf p =
  lexbuf.pos_mark <- lexbuf.pos;
  lexbuf.last_char_mark <- lexbuf.last_char;
  Sedlexing.mark lexbuf.buf p

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.pos_mark;
  lexbuf.last_char <- lexbuf.last_char_mark;
  Sedlexing.backtrack lexbuf.buf

let start lexbuf =
  lexbuf.pos_mark <- lexbuf.pos;
  lexbuf.last_char_mark <- lexbuf.last_char;
  Sedlexing.start lexbuf.buf

(** location of next character *)
let next_loc lexbuf =
  { lexbuf.pos with pos_cnum = lexbuf.pos.pos_cnum + 1 }

let cr = Char.to_int '\r'

(** next character *)
let next lexbuf =
  let c = Sedlexing.next lexbuf.buf in
  let pos = next_loc lexbuf in
  (match Char.of_int c with
  | Some '\r' ->
    lexbuf.pos <- { pos with 
      pos_bol = pos.pos_cnum - 1;
      pos_lnum = pos.pos_lnum + 1; }
  | Some '\n' when not (lexbuf.last_char = Some cr) ->
    lexbuf.pos <- { pos with 
      pos_bol = pos.pos_cnum - 1;
      pos_lnum = pos.pos_lnum + 1; }
  | Some '\n' -> ()
  | _ -> lexbuf.pos <- pos);
  lexbuf.last_char <- Some c;
  c

let raw lexbuf : int array =
  Sedlexing.lexeme lexbuf.buf

let ascii ?(skip=0) ?(drop=0) lexbuf : string =
  let len = Sedlexing.(lexeme_length lexbuf.buf - skip - drop) in
  Sedlexing.(Latin1.sub_lexeme lexbuf.buf skip len)

