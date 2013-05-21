{
  open Lexing
  open Mininet_Parser
}

let blank = [ ' ' '\t'  ]

(* dec_digit+ corresponds to DecimalDigits in the spec. *)
let dec_digit = ['0'-'9']

let int64 = dec_digit+

let int = dec_digit+

rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "<->" { LINKS }
   | 's' (int64 as sw) { SWITCH (Int64.of_string sw) }
   | 's' (int64 as sw) "-eth" (int as pt) 
       { TOSWITCH (Int64.of_string sw, int_of_string pt) }
   (* If a has several eth cards, they all have their own port numbers.
      But, I assume this doesn't matter for policies. *)
   | 'h' (int64 as mac) "-eth" int
       { TOHOST (Int64.of_string mac) }
   | eof { EOF }
