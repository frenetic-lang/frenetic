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
open Frenetic_NetKAT
#ifdef EXT
open Ast_convenience
#endif
%}

(* predicates and policies *)
%token TRUE FALSE AND OR NOT EQUALS
%token ID DROP FILTER ASSIGN SEMICOLON PLUS STAR LINK AT SLASH
%token IF THEN ELSE WHILE DO

(* fields *)
%token SWITCH PORT ETHSRC ETHDST VLAN VLANPCP ETHTYPE IPPROTO IP4SRC IP4DST TCPSRCPORT TCPDSTPORT
%token <string> QUERY PIPE

(* values *)
%token <string> INT IP4ADDR

(* primitives *)
%token LPAR RPAR BEGIN END EOF

(* antiquotations (for ppx syntax extension ) *)
%token <string> ANTIQ

(* precedence and associativity - from lowest to highest *)
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
      PPX( Frenetic_NetKAT.([%e p]))
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
      AST( Modify hv )
      PPX( Modify [%e hv] )
  | p=pol; PLUS; q=pol
      AST( Union (p, q) )
      PPX( Union ([%e p], [%e q]) )
  | p=pol; SEMICOLON; q=pol
      AST( Seq (p, q) )
      PPX( Seq ([%e p], [%e q]) )
  | p=pol; STAR
      AST( Star p )
      PPX( Star [%e p] )
  | sw1=int64; AT; pt1=int32; LINK; sw2=int64; AT; pt2=int32
      AST( Link (sw1, pt1, sw2, pt2) )
      PPX( Link ([%e sw1], [%e pt1], [%e sw2], [%e pt2]) )
  | IF; a=pred; THEN; p=pol; ELSE; q=pol 
      AST( Union (Seq (Filter a, p), Seq (Filter (Neg a), q)) )
      PPX( Union (Seq (Filter [%e a], [%e p]), Seq (Filter (Neg [%e a]), [%e q])) )
  | WHILE; a=pred; DO; p=pol
      AST( Seq (Star (Seq (a, p)), Neq a) )
      PPX( Seq(Star (Seq ([%e a], [%e p])), Neg [%e a]) )
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
  | PORT; BINOP; s=PIPE
      AST( Location (Pipe s) )
      PPX( Location (Pipe [%e str s]) )
  | PORT; BINOP; s=QUERY
      AST( Location (Query s) )
      PPX( Location (Query [%e str s]) )
  | VLAN; BINOP; n=int
      AST( Vlan n )
      PPX( Vlan [%e n] )
  | VLANPCP; BINOP; n=int
      AST( VlanPcp n )
      PPX( VlanPcp [%e n] )
  | ETHTYPE; BINOP; n=int
      AST( EthType n )
      PPX( EthType [%e n] )
  | ETHSRC; BINOP; n=int64
      AST( EthSrc n )
      PPX( EthSrc [%e n] )
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
      AST( Ip4Src (n,m) )
      PPX( Ip4Src [%e n] )
  | IP4DST; BINOP; n=ip4addr; m=ipmask
      AST( Ip4Dst n )
      PPX( Ip4Dst ([%e n],[%e m]) )
  AQ


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
      AST( Int64.of_string n )
      PPX( [%e int64 (Int32.of_string n) )
  AQ
  ;

ip4addr:
  | s = IP4ADDR
      AST( Ipaddr.V4.(to_int32 (of_string_exn s)) )
      PPX( [%expr Ipaddr.V4.(to_int32 (of_string_exn [%e str s]))] )
  ;

ipmask:
  | SLASH; m=int32 { m }
  | AST( 32 ) PPX( [%expr 32] )
  ;
