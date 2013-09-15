%{
  open Types

  let hex_of_int len n =
    let rec loop i n acc =
      if i <= 0 then acc
      else
      let n' = n lsr 4 in
      loop (pred i) n' ((n mod 16)::acc) in
    if 0 > n || n > 16 lsl (pred len * 4) then
      failwith (Printf.sprintf "Error: n=%d len=%d" n len)
    else
      loop len n []


 let int8_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 &&
     Int64.compare n (Int64.of_int 0xFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let int12_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 &&
     Int64.compare n (Int64.of_int 0xFFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let int16_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 &&
     Int64.compare n (Int64.of_int 0xFFFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let int32_of_int64 (n : Int64.t) : int32 =
   if Int64.compare n Int64.zero >= 0 &&
     Int64.compare n (Int64.of_int32 Int32.max_int) <= 0 then
     (Int32.of_int (Int64.to_int n))
   else
     raise Parsing.Parse_error

 let int_of_int64 (n : Int64.t) : int =
   if Int64.compare n (Int64.of_int max_int) <= 0 &&
     Int64.compare n (Int64.of_int min_int) >= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let mk_node i =
     "h" ^ (Int64.to_string i)

 let blank_nattr = {
   kind = "host"
   ; id = 0L
 }

 let update_nattr attr a =
   match a with
     | Kind(s) -> { attr with kind = s }
     | Id(i) -> { attr with id = i }

 let blank_eattr = {
   sport = 0l
   ; dport = 0l
   ; label = ""
   ; cost  = 0L
   ; capacity = 134217728L
 }

 let update_eattr attr a =
   match a with
     | SPort(p) -> {attr with sport = p}
     | DPort(p) -> {attr with dport = p}
     | Label(l) -> {attr with label = l }
     | Cost(c) -> {attr with cost = c}
     | Capacity(c) -> {attr with capacity = c}

%}

%token<Types.info> EOF NEWLINE
%token<Types.info> ARROW MAX MIN

%token<Types.info> LANGLE RANGLE LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE SEMI
%token<Types.info> EQUALS LEQ GEQ AMP BAR NOT TILDE BACKSLASH COMMA PLUS MINUS STAR DOT COLON
%token<Types.info * int64> INT64 HEX MACADDR
%token<Types.info * int32> IPADDR
%token<Types.info * float> FLOAT
%token<Types.info * string> STRING IDENT
%token<Types.info> STRICT GRAPH DIGRAPH SPORT DPORT TYPE LABEL COST ID CAPACITY

%left BAR
%left STAR
%right NOT

%type <Types.dotgraph> graph

%start graph

%%


/* ----- DOT GRAPH LANGUAGE SPECIFICATION ----- */
graph:
 | GRAPH name LBRACE stmts RBRACE
     { DotGraph($2, $4) }
 | DIGRAPH name LBRACE distmts RBRACE
     { DotDigraph($2, $4) }
 | STRICT GRAPH name LBRACE stmts RBRACE
     { DotGraph($3, $5) }
 | STRICT DIGRAPH name LBRACE distmts RBRACE
     { DotDigraph($3, $5) }

name:
 | IDENT
     { let _,i = $1 in i}
 | STRING
     { let _,i = $1 in i}

stmts:
 | stmt stmts
     { $1::$2}
 |
     { [] }

stmt:
 | edge SEMI
     { $1 }
 | node SEMI
     { $1 }

distmts:
 | distmt distmts
     { $1::$2}
 |
     { [] }

distmt:
 | diedge SEMI
     { $1 }
 | node SEMI
     { $1 }

edge:
 | IDENT MINUS MINUS IDENT eattrls
     {
       let _,s = $1 in
       let _,d = $4 in
       DotEdge (s, d, $5)
     }
 | INT64 MINUS MINUS INT64 eattrls
     {
       let _,s = $1 in
       let _,d = $4 in
       DotEdge (mk_node s, mk_node d, $5)
     }

diedge:
 | IDENT ARROW IDENT eattrls
     {
       let _,s = $1 in
       let _,d = $3 in
       DotDiedge (s, d, $4)
     }
 | INT64 ARROW INT64 eattrls
     {
       let _,s = $1 in
       let _,d = $3 in
       DotDiedge (mk_node s, mk_node d, $4)
     }

eattrls:
 | LBRACK eattrs RBRACK
     { $2 }
 |
     { blank_eattr }

eattrs:
 | eattr COMMA eattrs
     { update_eattr $3 $1 }
 | eattr
     { update_eattr blank_eattr $1 }
 |
     { blank_eattr }

eattr:
 | DPORT EQUALS INT64
     { let _,i = $3 in DPort (int32_of_int64 i) }
 | SPORT EQUALS INT64
     { let _,i = $3 in SPort (int32_of_int64 i) }
 | LABEL EQUALS STRING
     { let _,s = $3 in Label s }
 | COST EQUALS INT64
     { let _,c = $3 in Cost c }
 | CAPACITY EQUALS rate
     { Capacity $3 }

rate:
   | INT64 IDENT
      { let _,n = $1 in
        let _,b = $2 in
        let m =
          match b with
          | "Bps" -> 1L
          | "kbps" -> 128L
          | "kBps" -> 1024L
          | "Mbps" -> 131072L
          | "MBps" -> 1048576L
          | "Gbps" -> 134217728L
          | "GBps" -> 1073741824L
          | "Tbps" -> 137438953472L
          | "TBps" -> 1099511627776L
          | _ -> raise Parse_error in
        Int64.mul n  m
      }

node:
 | IDENT nattrls
     { let _,i = $1 in DotNode(i, $2) }
 | INT64 nattrls
     { let _,i = $1 in DotNode(mk_node i, $2)}

nattrls:
 | LBRACK nattrs RBRACK
     { $2 }
 |
     { blank_nattr }

nattrs:
 | nattr COMMA nattrs
     { update_nattr $3 $1 }
 | nattr
     { update_nattr blank_nattr $1 }
 |
     { blank_nattr }

nattr:
 | TYPE EQUALS STRING
     { let _,s = $3 in Kind s }
 | ID EQUALS INT64
     { let _,i = $3 in Id i }
