(* Adapted from https://github.com/jaked/ambassadortothecomputers.blogspot.com/blob/4d1bde223b1788ba52cc0f74b256760d9c059ac4/_code/camlp4-custom-lexers/jq_lexer.ml 
   This module is shared by Frenetic_NetKAT_Parser and Frenetic_Syntax_Extension_Parser
*)

open Core.Std

module Loc = Camlp4.PreCast.Loc

module Error =
struct
  type t = string
  exception E of string
  let print = Format.pp_print_string
  let to_string x = x
end
let _ = let module M = Camlp4.ErrorHandler.Register(Error) in ()

type token =
  | KEYWORD  of string
  | INT of string
  | INT32 of string
  | INT64 of string
  | IP4ADDR of string
  (* | ANTIQUOT of string *)
  | EOI

module Token =
struct
  module Loc = Loc
  module Error = Error

  type t = token

  let to_string t =
    let sf = Printf.sprintf in
    match t with
      | KEYWORD s -> sf "KEYWORD %s" s
      | IP4ADDR s -> sf "IP4ADDR %s" s
      | INT s -> sf "INT %s" s
      | INT32 s -> sf "INT32 %s" s
      | INT64 s -> sf "INT64 %s" s
      (* TODO: See below on why Antiquotes don't work
      | ANTIQUOT s -> sf "ANTIQUOT %s" s
    *)
      | EOI             -> sf "EOI"

  let print ppf x = Format.pp_print_string ppf (to_string x)

  let match_keyword kwd =
    function
      | KEYWORD kwd' when kwd = kwd' -> true
      | _ -> false

  let extract_string =
    function
      | KEYWORD s | INT s | INT64 s | INT32 s | IP4ADDR s -> s
      | tok ->
          invalid_arg
            ("Cannot extract a string from this token: " ^
               to_string tok)

  module Filter =
  struct
    type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
    type t = unit
    let mk _ = ()
    let filter _ strm = strm
    let define_filter _ _ = ()
    let keyword_added _ _ _ = ()
    let keyword_removed _ _ = ()
  end

end

module L = Ulexing

type context = {
  mutable loc : Loc.t;
  mutable start_loc : Loc.t option; (* if set, start lexeme here *)
  antiquots   : bool;
  lexbuf      : Ulexing.lexbuf;
  enc         : Ulexing.enc ref;
}

(* TODO: This is the same as in Frenetic_NetKAT_Json ... factor into better place *)
let macaddr_from_string (str : string) : Int64.t =
  let buf = Macaddr.to_bytes (Macaddr.of_string_exn str) in
  let byte n = Int64.of_int (Char.to_int (Bytes.get buf n)) in
  let rec loop n acc =
    let shift = 8 * (5 - n) in
    let acc' = Int64.(acc + (shift_left (byte n) shift)) in
    if n = 5 then acc'
    else loop (n + 1) acc' in
  loop 0 0L

let current_loc c =
  let (fn, bl, bb, bo, el, eb, _, g) = Loc.to_tuple c.loc in
  let bl, bb, bo =
    match c.start_loc with
      | Some loc ->
          let (_, bl, bb, bo, _, _, _, _) = Loc.to_tuple loc in
          bl, bb, bo
      | None -> bl, bb, Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g);
  c.start_loc <- None;
  c.loc

let set_start_loc c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bo = Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.start_loc <- Some (Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g))

let next_line c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bl = bl + 1 in
  let el = el + 1 in
  let bb = Ulexing.lexeme_end c.lexbuf in
  let eb = bb in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g)

let error c s = Loc.raise (current_loc c) (Error.E s)

let regexp identinit =
  ['A'-'Z' 'a'-'z' '_' ]
let regexp identchar = (identinit | [".'_" ] | [ '0'-'9' ])
let regexp ident = identinit identchar*
let regexp hex = ['0'-'9''a'-'f''A'-'F']
let regexp hexnum = '0' 'x' hex+
let regexp decnum = ['0'-'9']+
let regexp decbyte = (['0'-'9'] ['0'-'9'] ['0'-'9']) | (['0'-'9'] ['0'-'9']) | ['0'-'9']
let regexp hexbyte = hex hex

let regexp newline = ('\010' | '\013' | "\013\010")
let regexp blank = [' ' '\009']

let illegal c = error c "Illegal character in NetKAT expression"

let rec token c = lexer
  | ">>" -> EOI
  | eof -> EOI
  | newline -> next_line c; token c c.lexbuf
  | blank+ -> token c c.lexbuf
  | decbyte '.' decbyte '.' decbyte '.' decbyte -> IP4ADDR (L.latin1_lexeme c.lexbuf)
  | hexbyte ':' hexbyte ':' hexbyte ':' hexbyte ':' hexbyte ':' hexbyte ->
    INT64 (Int64.to_string(macaddr_from_string (L.latin1_lexeme c.lexbuf))) 
  | (hexnum | decnum)  -> INT (L.latin1_lexeme c.lexbuf)
  | (hexnum | decnum) 'l' -> INT32 (L.latin1_lexeme c.lexbuf)
  | (hexnum | decnum) 'L' -> INT64 (L.latin1_lexeme c.lexbuf)
  (* TODO: Thought this was causing parsing errors, but I was mistaken.  Re-enable.
  | "$" ident ->
     ANTIQUOT( L.latin1_sub_lexeme c.lexbuf 1 (L.lexeme_length c.lexbuf - 1))
   *)
  | "(*" -> 
    set_start_loc c;
    let _ = comment c lexbuf in
    token c c.lexbuf 
  | [ "()!+;=*+/|@" ] | ":=" | "=>" | "=>>"
    | "true" | "false" | "switch" | "port" | "vswitch" | "vport" | "vlanId"
    | "vlanPcp" | "ethTyp" | "ipProto" | "tcpSrcPort" | "tcpDstPort"
    | "ethSrc" | "ethDst" | "ip4Src"| "ip4Dst" | "and" | "or" | "not" | "id"
    | "drop" | "if" | "then" | "else" | "filter"  ->
      Printf.printf "GOT A LEXEME[%s]\n%!" (L.latin1_lexeme c.lexbuf);
      KEYWORD (L.latin1_lexeme c.lexbuf)
  | _ -> illegal c

(* Swallow all characters in comments *)
and comment c = lexer
  | eof -> error c "Unterminated comment"
  | "*)" -> ()
  | _ -> comment c c.lexbuf


let mk () start_loc cs =
  let enc = ref Ulexing.Latin1 in
  let lb = L.from_var_enc_stream enc cs in
  let c = {
    loc        = start_loc;
    start_loc  = None;
    antiquots  = !Camlp4_config.antiquotations;
    lexbuf     = lb;
    enc        = enc;
  } in
  let next _ =
    let tok =
      try token c c.lexbuf
      with
        | Ulexing.Error -> error c "Unexpected character"
        | Ulexing.InvalidCodepoint i -> error c "Code point invalid for the current encoding"
    in
    Some (tok, current_loc c)
  in
  Stream.from next
