{
  open Lexing
  open NetKAT_Parser

  let parse_byte str = Int64.of_string ("0x" ^ str)
  let parse_decbyte str = Int64.of_string str

  type mode =
    | Code
    | LiterateLine
    | LiterateBlock

  let st = ref Code
}

let blank = [ ' ' '\t'  ]

let id      = ['a'-'z' 'A'-'z' '_']['a'-'z' 'A'-'z' '0'-'9' '_']*
let decimal = ['0'-'9']+
let float_  = ['0'-'9']+ '.' ['0'-'9']+
let hex     = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let byte    = ['0'-'9' 'a'-'f' 'A'-'F']?  ['0'-'9' 'a'-'f' 'A'-'F']
let decbyte = 
  (['0'-'9'] ['0'-'9'] ['0'-'9']) | (['0'-'9'] ['0'-'9']) | ['0'-'9']
let string_body = ([^'"'] | "\\\"")*

rule literate = parse
  | "    "  { st := LiterateLine; token lexbuf }
  | "```\n" { st := LiterateBlock; token lexbuf }
  | '\n'    { new_line lexbuf; literate lexbuf }
  | _       { literate_text lexbuf }

(* preserves !st *)
and literate_text = parse
  | '\n'      { new_line lexbuf; literate lexbuf }
  | eof       { EOF }
  | [^ '\n']  { literate_text lexbuf }

and token = parse
  | "(*"    { block_comment lexbuf }
  | blank+  { token lexbuf }
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
  | "in" { IN }
  | "at" { AT }
  | "publicIP" { PUBLICIP } 
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "!" { NOT }
  | "?" { QMARK }
  | "*" { STAR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "all" { ALL }
  | "fwd" { FWD }
  | "<none>" { NONE }
  | "filter" { FILTER }
  | "=" { EQUALS }
  | ":=" { ASSIGN }
  | "switch" { SWITCH }
  | "include" { INCLUDE }
  | "check" { CHECK }
  | "vlan" { VLAN }
  | "dlSrc" { SRCMAC }
  | "dlDst" { DSTMAC }
  | "srcIP" { SRCIP }
  | "dstIP" { DSTIP }
  | "nwProto" { PROTOCOLTYPE }
  | "tcpSrcPort" { TCPSRCPORT }
  | "tcpDstPort" { TCPDSTPORT }
  | "dlTyp" { FRAMETYPE }
  | "arp" { ARP }
  | "ip" { IP }
  | "icmp" { ICMP }
  | "tcp" { TCP }
  | "udp" { UDP }
  | "port" { PORT }
  | "&&" { AND }
  | "||" { OR }
  | "begin" { BEGIN }
  | "end" { END }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "id" { ID }
  | "drop" { DROP }
  | "monitorPolicy" { MONITOR_POL }
  | "monitorTable" { MONITOR_TBL }
  | "monitorLoad" { MONITOR_LOAD }
  | "monitorPackets" { MONITOR_PKTS }
  | "fw" { FW }
  | ";" { SEMI }
  | "|" { BAR }
  | "+" { PLUS }

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
    { let open Int64 in
      IPADDR 
        (logor (shift_left (parse_decbyte b4) 24)
           (logor (shift_left (parse_decbyte b3) 16)
              (logor (shift_left (parse_decbyte b2) 8)
                 (parse_decbyte b1)))) }

  | decimal as n { INT64 (Int64.of_string n) } 
  | hex as n { INT64 (Int64.of_string n) }
  | "let" { LET }
  | '"' (string_body as s) '"' { STRING s }

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
