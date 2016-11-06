(* if EXT is set, produce OCaml (PPX) AST; otherwise, produce normal NetKAT AST *)
#ifdef EXT
  #define AST(arg)
  #define PPX(arg) {[%expr arg]}
  #define AQ | s=ANTIQ { Ast_convenience.evar s }
  #define POLTY Parsetree.expression
  #define PREDTY Parsetree.expression
#else
  #define AST(arg) {arg}
  #define PPX(arg)
  #define AQ
  #define POLTY Frenetic_NetKAT.policy
  #define PREDTY Frenetic_NetKAT.pred
#endif

%{
open Core.Std
open Frenetic_NetKAT
#ifdef EXT
open Parsetree
open Ast_helper
open Ast_convenience
(* FIXME: temporary, until these are included in the next Ast_convenience release *)
let int32 ?loc ?attrs x =
  Exp.constant ?loc ?attrs (Pconst_integer (Int32.to_string x, Some 'l'))
let int64 ?loc ?attrs x =
  Exp.constant ?loc ?attrs (Pconst_integer (Int64.to_string x, Some 'L'))
#endif
%}

(* predicates and policies *)
%token TRUE FALSE AND OR NOT EQUALS
%token ID DROP FILTER ASSIGN SEMICOLON PLUS STAR LINK AT SLASH
%token IF THEN ELSE WHILE DO

(* fields *)
%token SWITCH PORT ETHSRC ETHDST VLAN VLANPCP ETHTYPE IPPROTO IP4SRC IP4DST TCPSRCPORT TCPDSTPORT

(* meta fields *)
%token <string> METAID
%token LET VAR IN

(* values *)
%token QUERY PIPE
%token <string> INT IP4ADDR MAC STRING

(* primitives *)
%token LPAR RPAR BEGIN END EOF

(* antiquotations (for ppx syntax extension ) *)
%token <string> ANTIQ

(* precedence and associativity - from lowest to highest *)
%nonassoc IN (* let meta := 1 in p + q == let meta := 1 in (p + q) *)
%left PLUS
%left SEMICOLON
%nonassoc ELSE (* if a then p else q; r == (if a then p else q); r *)
%nonassoc DO
%nonassoc STAR (* if a then p else q* == if a then p else (q* ) *)

%left OR
%left AND
%nonassoc NOT

%start <POLTY> pol_eof
%start <PREDTY> pred_eof

%%
pol_eof:
  | p=pol; EOF
      AST( p )
      PPX( [%e p] )
  ;

pred_eof:
  | a=pred; EOF { a }
  ;

pol:
  | DROP
      AST( drop )
      PPX( drop )
  | ID
      AST( id )
      PPX( id )
  | FILTER; a=pred
      AST( Filter a )
      PPX( Filter [%e a] )
  | hv=header_val(ASSIGN)
      AST( Mod hv )
      PPX( Mod [%e hv] )
  | p=pol; PLUS; q=pol
      AST( Union (p, q) )
      PPX( Union ([%e p], [%e q]) )
  | p=pol; SEMICOLON; q=pol
      AST( Seq (p, q) )
      PPX( Seq ([%e p], [%e q]) )
  | p=pol; STAR
      AST( Star p )
      PPX( Star [%e p] )
  | LET; id=METAID; ASSIGN; v=metaval; IN; p=pol
      AST( Let(id         , Const v.    , false, p.    ) )
      PPX( Let([%e str id], Const [%e v], false, [%e p]) )
  | VAR; id=METAID; ASSIGN; v=metaval; IN; p=pol
      AST( Let(id         , Const v.    , true, p.    ) )
      PPX( Let([%e str id], Const [%e v], true, [%e p]) )
  | sw1=int64; AT; pt1=int32; LINK; sw2=int64; AT; pt2=int32
      AST( Link (sw1, pt1, sw2, pt2) )
      PPX( Link ([%e sw1], [%e pt1], [%e sw2], [%e pt2]) )
  | IF; a=pred; THEN; p=pol; ELSE; q=pol 
      AST( Union (Seq (Filter a, p)          , Seq (Filter (Neg a)     , q)) )
      PPX( Union (Seq (Filter [%e a], [%e p]), Seq (Filter (Neg [%e a]), [%e q])) )
  | WHILE; a=pred; DO; p=pol
      AST( Seq (Star (Seq (Filter a, p))          , Filter (Neg a)) )
      PPX( Seq (Star (Seq (Filter [%e a], [%e p])), Filter (Neg [%e a])) )
  | LPAR; p=pol; RPAR
      { p }
  | BEGIN p=pol; END
      { p }
  AQ
  ;


pred:
  | FALSE
      AST( False )
      PPX( False )
  | TRUE
      AST( True )
      PPX( True )
  | hv=header_val(EQUALS)
      AST( Test hv )
      PPX( Test [%e hv] )
  | NOT; a=pred
      AST( Neg a )
      PPX( Neg [%e a] )
  | a=pred; OR; b=pred
      AST( Or(a, b) )
      PPX( Or([%e a], [%e b]) )
  | a=pred; AND; b=pred
      AST( And(a, b) )
      PPX( And([%e a], [%e b]) )
  | LPAR; a=pred; RPAR
      { a }
  | BEGIN a=pred; END
      { a }
  AQ
  ;


(*********************** HEADER VALUES *************************)
(* FIXME: add local fields (aka meta fields) back to the language! *)

header_val(BINOP):
  | SWITCH; BINOP; n=int64
      AST( Switch n )
      PPX( Switch [%e n])
  | PORT; BINOP; n=int32
      AST( Location (Physical n) )
      PPX( Location (Physical [%e n]) )
  | PORT; BINOP; s=pipe
      AST( Location (Pipe s) )
      PPX( Location (Pipe [%e s]) )
  | PORT; BINOP; s=query
      AST( Location (Query s) )
      PPX( Location (Query [%e s]) )
  | VLAN; BINOP; n=int
      AST( Vlan n )
      PPX( Vlan [%e n] )
  | VLANPCP; BINOP; n=int
      AST( VlanPcp n )
      PPX( VlanPcp [%e n] )
  | ETHTYPE; BINOP; n=int
      AST( EthType n )
      PPX( EthType [%e n] )
  | ETHSRC; BINOP; m=mac
      AST( EthSrc m )
      PPX( EthSrc [%e m] )
  | ETHSRC; BINOP; n=int64
      AST( EthSrc n )
      PPX( EthSrc [%e n] )
  | ETHDST; BINOP; m=mac
      AST( EthDst m )
      PPX( EthDst [%e m] )
  | ETHDST; BINOP; n=int64
      AST( EthDst n )
      PPX( EthDst [%e n] )
  | TCPSRCPORT; BINOP; n=int
      AST( VlanPcp n )
      PPX( VlanPcp [%e n] )
  | TCPDSTPORT; BINOP; n=int
      AST( VlanPcp n )
      PPX( VlanPcp [%e n] )
  | IPPROTO; BINOP; n=int
      AST( IPProto n )
      PPX( IPProto [%e n] )
  | IP4SRC; BINOP; n=ip4addr; m=ipmask
      AST( IP4Src (n,m) )
      PPX( IP4Src ([%e n],[%e m]) )
  | IP4DST; BINOP; n=ip4addr; m=ipmask
      AST( IP4Dst (n,m) )
      PPX( IP4Dst ([%e n],[%e m]) )
  | id=METAID; BINOP; n=metaval
      AST( Meta (id, n) )
      PPX( Meta ([%e str id], [%e n]) )
  ;


(*********************** VALUES *************************)
(* FIXME: we should be parsing *unsigned* int32 and int64 here! *)

int:
  | n=INT
      AST( Int.of_string n )
      PPX( [%e int (Int.of_string n)] )
  AQ
  ;

int32:
  | n=INT
      AST( Int32.of_string n )
      PPX( [%e int32 (Int32.of_string n)] )
  AQ
  ;

int64:
  | n=INT
      AST(            Int64.of_string n )
      PPX( [%e int64 (Int64.of_string n)] )
  AQ
  ;

mac:
  | s = MAC

      AST( Frenetic_Packet.mac_of_string         s  )
      PPX( Frenetic_Packet.mac_of_string [%e str s] )
  ;

ip4addr:
  | s = IP4ADDR
      AST( Ipaddr.V4.(to_int32 (of_string_exn         s )) )
      PPX( Ipaddr.V4.(to_int32 (of_string_exn [%e str s])) )
  ;

ipmask:
  | SLASH; m=int32 { m }
  | AST( 32l ) PPX( 32l )
  ;

pipe:
  | PIPE; LPAR; s=STRING; RPAR
      AST( s )
      PPX( [%e str s] )

query:
  | QUERY; LPAR; s=STRING; RPAR
      AST( s )
      PPX( [%e str s] )

metaval:
  | n=int64 { n }
  | m=mac { m }
  | ip=ip4addr
    AST( Int64.of_int32 ip )
    PPX( Int64.of_int32 [%e ip] )
  ;

%%

