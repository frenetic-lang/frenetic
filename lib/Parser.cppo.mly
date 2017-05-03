(* if MAKE_PPX is set, produce OCaml (PPX) AST; otherwise, produce normal NetKAT AST *)
#ifdef MAKE_PPX
  #define AST(arg)
  #define PPX(arg) {[%expr arg]}
  #define AQ | x=ANTIQ { Ast_convenience.evar (fst x) ~loc:(snd x) }
  #define POLTY Parsetree.expression
  #define PREDTY Parsetree.expression
#else
  #define AST(arg) {arg}
  #define PPX(arg)
  #define AQ
  #define POLTY Frenetic_NetKAT.policy
  #define PREDTY Frenetic_NetKAT.pred
#endif
#define BOTH(arg) AST(arg) PPX(arg)

%{
open Core.Std
open Frenetic_NetKAT
#ifdef MAKE_PPX
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
%token ID DROP FILTER ASSIGN SEMICOLON PLUS STAR LINK VLINK AT SLASH
%token IF THEN ELSE WHILE DO
%token DUP

(* fields *)
%token SWITCH PORT VSWITCH VPORT VFABRIC ETHSRC ETHDST VLAN VLANPCP ETHTYPE IPPROTO IP4SRC IP4DST TCPSRCPORT TCPDSTPORT ABSTRACTLOC FROM SWITCHPREFIX HOSTPREFIX

(* meta fields *)
%token <string> METAID
%token LET VAR IN

(* values *)
%token QUERY PIPE
%token <string> INT IP4ADDR MAC STRING

(* primitives *)
%token LPAR RPAR BEGIN END EOF

(* antiquotations (for ppx syntax extension ) *)
#ifdef MAKE_PPX
%token <string * Location.t> ANTIQ
#endif

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
      PPX( let open Frenetic_NetKAT in [%e p] )
  ;

pred_eof:
  | a=pred; EOF
      AST( a )
      PPX( let open Frenetic_NetKAT in [%e a] )
  ;

pol:
  | DROP
      AST( drop )
      PPX( drop )
  | ID
      AST( id )
      PPX( id )
  | DUP
      BOTH( Dup )
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
  | mut=letexp; id=METAID; ASSIGN; v=metaval; IN; p=pol
      AST( Let(id         , Const v     , mut     , p     ) )
      PPX( Let([%e str id], Const [%e v], [%e mut], [%e p]) )
  | mut=letexp; id=METAID; ASSIGN; a=alias; IN; p=pol
      AST( Let(id         , Alias a     , mut    , p     ) )
      PPX( Let([%e str id], Alias [%e a], [%e mut], [%e p]) )
  | sw1=int64; AT; pt1=int32; LINK; sw2=int64; AT; pt2=int32
      AST( Link (sw1, pt1, sw2, pt2) )
      PPX( Link ([%e sw1], [%e pt1], [%e sw2], [%e pt2]) )
  | sw1=int64; AT; pt1=int64; VLINK; sw2=int64; AT; pt2=int64
      AST( VLink (sw1, pt1, sw2, pt2) )
      PPX( VLink ([%e sw1], [%e pt1], [%e sw2], [%e pt2]) )
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

%inline
letexp:
  | LET BOTH( false )
  | VAR BOTH( true )

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

header_val(BINOP):
  | SWITCH; BINOP; n=int64
      AST( Switch n )
      PPX( Switch [%e n])
  | PORT; BINOP; p=portval
      AST( Location p )
      PPX( Location [%e p] )
  | VSWITCH; BINOP; n=int64
    AST( VSwitch n )
    PPX( VSwitch [%e n] )
  | VPORT; BINOP; n=int64
    AST( VPort n )
    PPX( VPort [%e n] )
  | VFABRIC; BINOP; n=int64
    AST( VFabric n )
    PPX( VFabric [%e n] )
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

(*********************** aliases *************************)
alias:
  | SWITCH
    BOTH( Switch (Obj.magic 0) )
  | PORT
    BOTH( Location (Obj.magic 0) )
  | VSWITCH
    BOTH( VSwitch (Obj.magic 0) )
  | VPORT
    BOTH( VPort (Obj.magic 0) )
  | VFABRIC
    BOTH( VFabric (Obj.magic 0) )
  | ETHSRC
    BOTH( EthSrc (Obj.magic 0) )
  | ETHDST
    BOTH( EthDst (Obj.magic 0) )
  | VLAN
    BOTH( Vlan (Obj.magic 0) )
  | VLANPCP
    BOTH( VlanPcp (Obj.magic 0) )
  | ETHTYPE
    BOTH( EthType (Obj.magic 0) )
  | TCPSRCPORT
    BOTH( TCPSrcPort (Obj.magic 0) )
  | TCPDSTPORT
    BOTH( TCPDstPort (Obj.magic 0) )
  | IPPROTO
    BOTH( IPProto (Obj.magic 0) )
  | IP4SRC
    BOTH( IP4Src (Obj.magic 0, Obj.magic 0) )
  | IP4DST
    BOTH( IP4Dst (Obj.magic 0, Obj.magic 0) )


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

portval:
  | n=int32
      AST( Physical n )
      PPX( Physical [%e n] )
  | PIPE; LPAR; s=STRING; RPAR
      AST( Pipe s )
      PPX( Pipe [%e str s] )
  | QUERY; LPAR; s=STRING; RPAR
      AST( Query s )
      PPX( Query [%e str s] )

metaval:
  | n=int64 { n }
  | m=mac { m }
  | ip=ip4addr
    AST( Int64.of_int32 ip )
    PPX( Int64.of_int32 [%e ip] )
  ;

%%
