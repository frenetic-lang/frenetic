{
open Frenetic_Decide_Parser
exception LexError of string
let parse_byte str = Int64.of_string ("0x" ^ str)
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let num = ['0'-'9']+
let whitespace = ['\t' '\r' ' ' '\n']
let byte = ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']



rule token = parse
  | whitespace { token lexbuf }
  | "drop" { ZERO }
  | "pass" { ONE }
  | "dup"  { DUP }
  | "and"  { AND }
  | "or"   { OR }
  | id as id { VAR id }
  | "\""   { STRING (String.concat "" (string lexbuf)) }
  | num as num { STRING (string_of_int (int_of_string num)) }
	| "(*"   { comment lexbuf }
  | ":="   { ASSG }
  | '+'    { PLUS }
  | ';'    { TIMES }
  | '*'    { STAR }
  | '~'    { NOT }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | ','    { COMMA }
  | "=="   { EQUIV }
  | '='    { EQ }
  | "!="   { NEQ }
  | "<="   { LE }
  | '<'    { LE }
  | (byte as n6) ":" (byte as n5) ":" (byte as n4) ":" (byte as n3) ":"
      (byte as n2) ":" (byte as n1)
      { let open Int64 in
	    STRING
	      (string_of_int (to_int (logor (shift_left (parse_byte n6) 40)
					(logor (shift_left (parse_byte n5) 32)
					   (logor (shift_left (parse_byte n4) 24)
					      (logor (shift_left (parse_byte n3) 16)
						 (logor (shift_left (parse_byte n2) 8)
						    (parse_byte n1)))))))) }
  | eof    { EOL }
  (* | '\n'   { EOL } *)
  | _ as c { raise (LexError ("Unexpected character " ^ (String.make 1 c))) }

and string = parse
  | "\\\\" { "\\" :: string lexbuf }
  | "\\\"" { "\"" :: string lexbuf }
  | "\\n"  { "\n" :: string lexbuf }
  | "\\t"  { "\t" :: string lexbuf }
  | "\""   { [] }
  | eof    { raise (LexError "Unexpected end of input stream") }
  | _ as c { String.make 1 c :: string lexbuf }

and comment = parse
  | "*)"   { token lexbuf }
  | eof    { raise (LexError "Unexpected end of input stream") }
  | _      { comment lexbuf }

