#define AST(arg) {arg}
#define POLTY Frenetic_NetKAT_Portless.policy
#define PREDTY Frenetic_NetKAT_Portless.pred

%{
open Core.Std
open Frenetic_NetKAT_Portless
%}

(* predicates and policies *)
%token TRUE FALSE AND OR NOT EQUALS
%token ID DROP FILTER ASSIGN SEMICOLON PLUS STAR AT SLASH
%token IF THEN ELSE WHILE DO

(* fields *)
%token ABSTRACTLOC FROM ETHSRC ETHDST VLAN VLANPCP ETHTYPE IPPROTO IP4SRC IP4DST TCPSRCPORT TCPDSTPORT SWITCHPREFIX HOSTPREFIX

(* values *)
%token <string> INT IP4ADDR MAC STRING

(* primitives *)
%token LPAR RPAR BEGIN END EOF

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
  ;

pred_eof:
  | a=pred; EOF
      AST( a )
  ;

pol:
  | DROP
      AST( drop )
  | ID
      AST( id )
  | FILTER; a=pred
      AST( Filter a )
  | hv=header_val(ASSIGN); SLASH; m=int
      AST( let (h,v) = hv in Mod (h, v, m) )
  | hv=header_val(ASSIGN)
      AST( let (h,v) = hv in Mod (h, v, 64) )
  | p=pol; PLUS; q=pol
      AST( Union (p, q) )
  | p=pol; SEMICOLON; q=pol
      AST( Seq (p, q) )
  | p=pol; STAR
      AST( Star p )
  | IF; a=pred; THEN; p=pol; ELSE; q=pol
      AST( Union (Seq (Filter a, p), Seq (Filter (Neg a)     , q)) )
  | WHILE; a=pred; DO; p=pol
      AST( Seq (Star (Seq (Filter a, p)) , Filter (Neg a)) )
  | LPAR; p=pol; RPAR
      { p }
  | BEGIN p=pol; END
      { p }
  ;

pred:
  | FALSE
      AST( False )
  | TRUE
      AST( True )
  | hv=header_val(EQUALS); SLASH; m=int
      AST( let (h,v) = hv in Test (h, v, m) )
  | hv=header_val(EQUALS)
      AST( let (h,v) = hv in Test (h, v, 64) )
  | NOT; a=pred
      AST( Neg a )
  | a=pred; OR; b=pred
      AST( Or(a, b) )
  | a=pred; AND; b=pred
      AST( And(a, b) )
  | LPAR; a=pred; RPAR
      { a }
  | BEGIN a=pred; END
      { a }
  ;

header_val(BINOP):
  | ETHSRC; BINOP; m=mac
      AST( (EthSrc, m) )
  | ETHDST; BINOP; m=mac
      AST( (EthDst, m) )
  | IP4SRC; BINOP; n=ip4addr
      AST( (IP4Src, n) )
  | IP4DST; BINOP; n=ip4addr
      AST( (IP4Dst, n) )
  | ABSTRACTLOC; BINOP; n=abstractloc
      AST( (AbstractLoc, n) )
  | FROM; BINOP; n=abstractloc
      AST( (From, n) )
  | h=header; BINOP; n=int64
      AST( (h, n) )
  ;

header:
  | VLAN
      AST( Vlan )
  | VLANPCP
      AST( VlanPcp )
  | ETHTYPE
      AST( EthType )
  | TCPSRCPORT
      AST( VlanPcp )
  | TCPDSTPORT
      AST( VlanPcp )
  | IPPROTO
      AST( IPProto )
  | ETHSRC
      AST( EthSrc )
  | ETHDST
      AST( EthDst )
  ;

(* FIXME: we should be parsing *unsigned* int32 and int64 here! *)

int:
  | n=INT
      AST( Int.of_string n )
  ;

int32:
  | n=INT
      AST( Int32.of_string n )
  ;

int64:
  | n=INT
      AST( Int64.of_string n )
  ;

ip4addr:
  | s=IP4ADDR
      AST( Int64.of_int32 (Ipaddr.V4.(to_int32 (of_string_exn s ))) )
  ;

mac:
  | s=MAC
      AST( Frenetic_Packet.mac_of_string s )
  ;

abstractloc:
  | SWITCHPREFIX; n=int64
      AST( Int64.shift_left n 1 )
  | HOSTPREFIX;   n=int64
      AST( Int64.(+) (Int64.shift_left n 1) 1L )
  ;

%%
