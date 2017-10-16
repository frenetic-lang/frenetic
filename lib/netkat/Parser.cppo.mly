(* if MAKE_PPX is set, produce OCaml (PPX) AST; otherwise, produce normal NetKAT AST *)
#ifdef MAKE_PPX
  #define AST(arg)
  #define LOC Location.{ loc_start = $symbolstartpos; loc_end = $endpos; loc_ghost = false}
  #define PPX(arg) {let loc = LOC in [%expr arg]}
  #define PPX_(arg) let loc = LOC in [%expr arg]
  #define AQ | x=ANTIQ { parse_ocaml_expr x }
  #define POLTY Ppx_core.Parsetree.expression
  #define PREDTY Ppx_core.Parsetree.expression
#else
  #define AST(arg) {arg}
  #define PPX(arg)
  #define AQ
  #define POLTY Syntax.policy
  #define PREDTY Syntax.pred
#endif
#define BOTH(arg) AST(arg) PPX(arg)

%{
(* ignore warnings in parser, since it is auto-generated *)
[@@@warning "-3-26"]
#ifdef MAKE_PPX
open Ppx_core
open Ast_builder.Default

let parse_ocaml_expr (s,loc) =
  let buf = Lexing.from_string s in
  buf.lex_start_p <- Location.(loc.loc_start);
  buf.lex_curr_p <- Location.(loc.loc_start);
  Parse.expression buf

open Frenetic_netkat.Syntax
#else
open Syntax
#endif

open Core
%}

(* predicates and policies *)
%token TRUE FALSE AND OR NOT EQUALS
%token ID DROP FILTER ASSIGN SEMICOLON PLUS STAR LINK VLINK AT SLASH
%token IF THEN ELSE WHILE DO
%token DUP

(* fields *)
%token SWITCH PORT VSWITCH VPORT VFABRIC ETHSRC ETHDST VLAN VLANPCP ETHTYPE IPPROTO IP4SRC IP4DST TCPSRCPORT TCPDSTPORT

(* meta fields *)
%token <string> METAID
%token LET VAR IN

(* values *)
%token QUERY PIPE
%token <string> INT IP4ADDR MAC STRING

(* primitives *)
%token LPAR RPAR BEGIN END EOF

(* portless extension *)
%token FROM ABSTRACTLOC

(* antiquotations (for ppx syntax extension ) *)
#ifdef MAKE_PPX
  %token <string * Location.t> ANTIQ
  (* iverson brackets *)
  %token <string * Location.t> IVERSON
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
      PPX( let open Frenetic_netkat.Syntax in [%e p] )
  ;

pred_eof:
  | a=pred; EOF
      AST( a )
      PPX( let open Frenetic_netkat.Syntax in [%e a] )
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
      AST( Let{id                     ; init=Const v     ; mut         ; body=p     } )
      PPX( Let{id=[%e estring ~loc id]; init=Const [%e v]; mut=[%e mut]; body=[%e p]} )
  | mut=letexp; id=METAID; ASSIGN; a=alias; IN; p=pol
      AST( Let{id                     ; init=Alias a     ; mut         ; body=p     } )
      PPX( Let{id=[%e estring ~loc id]; init=Alias [%e a]; mut=[%e mut]; body=[%e p]} )
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
#ifdef MAKE_PPX
  | code=IVERSON {
    let phi = parse_ocaml_expr code in
    PPX_( if [%e phi] then id else drop )
  }
#endif
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
#ifdef MAKE_PPX
  | code=IVERSON {
    let phi = parse_ocaml_expr code in
    PPX_( if [%e phi] then True else False )
  }
#endif
  ;


(*********************** HEADER VALUES *************************)

header_val(BINOP):
#ifdef PORTLESS
  | SWITCH; BINOP; n=int64
      BOTH( raise (Failure "cannot access switch field in portless mode") )
  | PORT; BINOP; _=portval
      BOTH( raise (Failure "cannot access port field in portless mode") )
  | FROM; BINOP; s=STRING
      BOTH( From s )
  | ABSTRACTLOC; BINOP; s=STRING
      BOTH( AbstractLoc s )
#else
  | SWITCH; BINOP; n=int64
      AST( Switch n )
      PPX( Switch [%e n])
  | PORT; BINOP; p=portval
      AST( Location p )
      PPX( Location [%e p] )
  | FROM; BINOP; _=STRING
      BOTH( raise (Failure "from field only available in portless mode") )
  | ABSTRACTLOC; BINOP; _=STRING
      BOTH( raise (Failure "loc field only available in portless mode") )
#endif
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
      PPX( Meta ([%e estring ~loc id], [%e n]) )
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
      PPX( [%e eint ~loc (Int.of_string n)] )
  AQ
  ;

int32:
  | n=INT
      AST( Int32.of_string n )
      PPX( [%e eint32 ~loc (Int32.of_string n)] )
  AQ
  ;

int64:
  | n=INT
      AST(            Int64.of_string n )
      PPX( [%e eint64 ~loc (Int64.of_string n)] )
  AQ
  ;

mac:
  | s = MAC

      AST( Frenetic_kernel.Packet.mac_of_string         s  )
      PPX( Frenetic_kernel.Packet.mac_of_string [%e estring ~loc s] )
  ;

ip4addr:
  | s = IP4ADDR
      AST( Ipaddr.V4.(to_int32 (of_string_exn         s )) )
      PPX( Ipaddr.V4.(to_int32 (of_string_exn [%e estring ~loc s])) )
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
      PPX( Pipe [%e estring ~loc s] )
  | QUERY; LPAR; s=STRING; RPAR
      AST( Query s )
      PPX( Query [%e estring ~loc s] )

metaval:
  | n=int64 { n }
  | m=mac { m }
  | ip=ip4addr
    AST( Int64.of_int32 ip )
    PPX( Int64.of_int32 [%e ip] )
  ;

%%
