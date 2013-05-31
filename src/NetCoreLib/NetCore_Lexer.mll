{
  open Lexing
  open NetCore_Parser

  let parse_byte str = Int64.of_string ("0x" ^ str)
  let parse_decbyte str = Int32.of_string str

  type mode =
    | Code
    | LiterateLine
    | LiterateBlock

  let st = ref Code
}

let blank = [ ' ' '\t'  ]

let id = ['a'-'z' 'A'-'z' '_']['a'-'z' 'A'-'z' '0'-'9' '_']*
let decimal = ['0'-'9']+
let float_ = ['0'-'9']+ '.' ['0'-'9']+
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let byte = ['0'-'9' 'a'-'f' 'A'-'F']  ['0'-'9' 'a'-'f' 'A'-'F']
let decbyte = 
  (['0'-'9'] ['0'-'9'] ['0'-'9']) | (['0'-'9'] ['0'-'9']) | ['0'-'9']

rule literate = parse
  | "    " { st := LiterateLine; token lexbuf }
  | "```\n" { st := LiterateBlock; token lexbuf }
  | '\n' { new_line lexbuf; literate lexbuf }
  | _ { literate_text lexbuf }

(* preserves !st *)
and literate_text = parse
  | '\n' { new_line lexbuf; literate lexbuf }
  | eof { EOF }
  | [^ '\n'] { literate_text lexbuf }

and token = parse
  | "(*" { block_comment lexbuf }
  | blank+ { token lexbuf }
  | "```\n" 
    { match !st with
      | LiterateBlock -> literate lexbuf 
      | Code -> TICKTICKTICK (* makes parser fail *)
      | LiterateLine -> TICKTICKTICK (* makes parser fail *) }
  | '\n' { new_line lexbuf; 
           match !st with
             | Code -> token lexbuf
             | LiterateBlock -> token lexbuf
             | LiterateLine -> literate lexbuf }
  | eof { EOF }
  | "," { COMMA }
  | "nat" { NAT }
  | "in" { IN }
  | "publicIP" { PUBLICIP } 
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "!" { NOT }
  | "*" { STAR }
  | "all" { ALL }
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
  | "frameType" { FRAMETYPE }
  | "arp" { ARP }
  | "ip" { IP }
  | "inPort" { INPORT }
  | "&&" { AND }
  | "||" { OR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "pass" { PASS }
  | "drop" { DROP }
  | "monitor_pol" { MONITOR_POL }
  | "monitor_tbl" { MONITOR_TBL }
  | "monitor_sw" { MONITOR_SW }
  | "monitor_load" { MONITOR_LOAD }
  | ";" { SEMI }
  | "|" { BAR }
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
  | float_ as f { FLOAT (float_of_string f) }
  | (decbyte as b4) "." (decbyte as b3) "." (decbyte as b2) "." (decbyte as b1)
    { let open Int32 in
      IPADDR 
        (logor (shift_left (parse_decbyte b4) 24)
           (logor (shift_left (parse_decbyte b3) 16)
              (logor (shift_left (parse_decbyte b2) 8)
                 (parse_decbyte b1)))) }
  | decimal as n { INT64 (Int64.of_string n) } 
  | hex as n { INT64 (Int64.of_string n) }
  | "learn" { LEARNING }
  | "let" { LET }
  | "publicIP" { PUBLICIP }
  | id as x { ID x } (* by going last, we lex to LEARN, NAT, etc. instead *)

and block_comment = parse
  | "*)" {  token lexbuf }
  | "*" { block_comment lexbuf }
  | [ '\n' ] { new_line lexbuf;
               match !st with
                 | Code -> block_comment lexbuf
                 | LiterateBlock -> block_comment lexbuf
                 | LiterateLine -> literate lexbuf
             }
  | ([^ '\n' '*'])+  { block_comment lexbuf }
