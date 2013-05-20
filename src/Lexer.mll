{
  open Lexing
  open Parser

  let parse_byte str = Int64.of_string ("0x" ^ str)
  let parse_decbyte str = Int32.of_string str

}

let blank = [ ' ' '\t'  ]

let decimal = ['0'-'9']+
let hex = ['0'-'9' 'a'-'f' 'A'-'F']+
let byte = ['0'-'9' 'a'-'f' 'A'-'F']  ['0'-'9' 'a'-'f' 'A'-'F']
let decbyte = ['0'-'9'] ['0'-'9'] ['0'-'9']

rule token = parse
  | blank+ { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | '\r' { new_line lexbuf; token lexbuf }
  | "\r\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "!" { NOT }
  | "*" { ALL }
  | "<none>" { NONE }
  | "=" { EQUALS }
  | "switch" { SWITCH }
  | "vlan" { VLAN }
  | "srcMac" { SRCMAC }
  | "dstMac" { DSTMAC }
  | "srcIP" { SRCIP }
  | "dstIP" { DSTIP }
  | "tcpSrcPort" { TCPSRCPORT }
  | "dstSrcPort" { TCPDSTPORT }
  | "inPort" { INPORT }
  | "&&" { AND }
  | "||" { OR }
  | "if" { IF }
  | "then" { THEN }
  | ";" { SEMI }
  | "|" { BAR }
  | decimal as n { INT64 (Int64.of_string n) }
  | hex as n { INT64 (Int64.of_string n) }
  | (byte as n6) ":" (byte as n5) ":" (byte as n4) ":" (byte as n3) ":" 
    (byte as n2) ":" (byte as n1) 
    { let open Int64 in
      MACADDR
        (logor (shift_left (parse_byte n6) 40)
           (logor (shift_left (parse_byte n5) 32)
              (logor (shift_left (parse_byte n4) 24)
                 (logor (shift_left (parse_byte n3) 16)
                    (logor (shift_left (parse_byte n2) 8)
                       (parse_byte n1)))))) }
  | (decbyte as b4) "." (decbyte as b3) "." (decbyte as b2) "." 
    (decbyte as b1)
    { let open Int32 in
      IPADDR 
        (logor (shift_left (parse_decbyte b4) 24)
           (logor (shift_left (parse_decbyte b3) 16)
              (logor (shift_left (parse_decbyte b2) 8)
                 (parse_decbyte b1)))) }
