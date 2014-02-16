%{
  open NetKAT_Types

  (* Ethernet frame types *)
  let arp : int64  = Int64.of_int 0x806
  let ip  : int64  = Int64.of_int 0x800

  (* Ip protocol types *)
  let icmp : int64 = Int64.of_int 0x01
  let tcp  : int64 = Int64.of_int 0x06
  let udp  : int64 = Int64.of_int 0x11

  (* hack for now *)
  let vlan_none : int64 = Int64.minus_one
%}

%token LPAREN RPAREN BEGIN END LCURLY RCURLY
%token DBLARROW
%token NOT QMARK
%token AND OR
%token TRUE FALSE
%token ALL FWD STAR
%token NONE
%token EQUALS
%token SWITCH PORT SRCMAC DSTMAC FRAMETYPE VLAN VLANPCP SRCIP DSTIP PROTOCOLTYPE TCPSRCPORT TCPDSTPORT
%token IF THEN ELSE
%token SEMI AMP BAR PLUS COMMA
%token LET IN
%token FILTER
%token ASSIGN
%token AT
%token PUBLICIP
%token ID DROP
%token ARP IP ICMP TCP UDP
%token MONITOR_POL MONITOR_TBL MONITOR_LOAD MONITOR_PKTS
%token <Int64.t> INT64
%token <Int64.t> MACADDR
%token <Int64.t> IPADDR
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token EOF

%start program

%type <NetKAT_Types.policy> program

%%

field_value :
  | INT64   
      { $1 }
  | MACADDR 
      { $1 }
  | NONE    
      { vlan_none }
  | IPADDR  
      { $1 }
  | ARP
      { arp }
  | IP
      { ip }
  | ICMP 
      { icmp }
  | TCP
      { tcp }
  | UDP
      { udp }
  
predicate :
  | predicate OR apredicate
      { Or ($1, $3) }
  | apredicate 
      { $1 }

apredicate:
  | apredicate AND upredicate
      { And ($1, $3) }
  | upredicate
      { $1 }
      
upredicate:
  | NOT upredicate 
      { Neg $2 }
  | xpredicate
      { $1 }

xpredicate:
  | LPAREN predicate RPAREN
      { $2 }
  | STAR
      { True }
  | NONE 
      { False }
  | SWITCH EQUALS field_value
      { Test(Switch $3) }
  | PORT EQUALS field_value
      { Test(Location(Physical (VInt.get_int32 (VInt.Int64 $3))))}
  | PORT EQUALS IDENT
      { Test(Location(Pipe $3)) }
  | SRCMAC EQUALS field_value
      { Test(EthSrc (VInt.get_int48 (VInt.Int64 $3))) }
  | DSTMAC EQUALS field_value
      { Test(EthDst (VInt.get_int48 (VInt.Int64 $3))) }
  | FRAMETYPE EQUALS field_value
      { Test(EthType (VInt.get_int16 (VInt.Int64 $3))) }
  | VLAN EQUALS field_value
      { Test(EthType (VInt.get_int16 (VInt.Int64 $3))) }
  | VLANPCP EQUALS field_value
      { Test(VlanPcp (VInt.get_int8 (VInt.Int64 $3))) }
  | SRCIP EQUALS field_value
      { Test(IP4Src (VInt.get_int32 (VInt.Int64 $3), 32)) }
  | DSTIP EQUALS field_value 
      { Test(IP4Dst (VInt.get_int32 (VInt.Int64 $3), 32)) }
  | PROTOCOLTYPE EQUALS field_value 
      { Test(IPProto (VInt.get_int8 (VInt.Int64 $3))) }
  | TCPSRCPORT EQUALS field_value 
      { Test(TCPSrcPort (VInt.get_int16 (VInt.Int64 $3))) }
  | TCPDSTPORT EQUALS field_value 
      { Test(TCPDstPort (VInt.get_int16 (VInt.Int64 $3))) }

/* TODO(jnf): should these be non-associative? */
policy : 
  | policy BAR spolicy
      { Union ($1, $3) }
  | spolicy 
      { $1 }

spolicy:
  | spolicy SEMI cpolicy 
      { Seq ($1, $3) }
  | cpolicy 
      { $1 }

cpolicy:
  | IF predicate THEN spolicy ELSE cpolicy 
      { Union(Seq(Filter $2, $4), Seq(Filter(Neg $2), $6)) }
  | kpolicy 
      { $1 }

kpolicy:
  | kpolicy STAR 
      { Star $1 }
  | xpolicy 
      { $1 }

xpolicy:
  | FILTER predicate 
      { Filter $2 }
  | PORT ASSIGN field_value
      { Mod(Location(Physical (VInt.get_int32 (VInt.Int64 $3))))}
  | PORT ASSIGN IDENT
      { Mod(Location(Pipe $3)) }
  | SRCMAC ASSIGN field_value
      { Mod(EthSrc (VInt.get_int48 (VInt.Int64 $3))) }
  | DSTMAC ASSIGN field_value
      { Mod(EthDst (VInt.get_int48 (VInt.Int64 $3))) }
  | FRAMETYPE ASSIGN field_value
      { Mod(EthType (VInt.get_int16 (VInt.Int64 $3))) }
  | VLAN ASSIGN field_value
      { Mod(EthType (VInt.get_int16 (VInt.Int64 $3))) }
  | VLANPCP ASSIGN field_value
      { Mod(VlanPcp (VInt.get_int8 (VInt.Int64 $3))) }
  | SRCIP ASSIGN field_value
      { Mod(IP4Src (VInt.get_int32 (VInt.Int64 $3),32)) }
  | DSTIP ASSIGN field_value 
      { Mod(IP4Dst (VInt.get_int32 (VInt.Int64 $3),32)) }
  | PROTOCOLTYPE ASSIGN field_value 
      { Mod(IPProto (VInt.get_int8 (VInt.Int64 $3))) }
  | TCPSRCPORT ASSIGN field_value 
      { Mod(TCPSrcPort (VInt.get_int16 (VInt.Int64 $3))) }
  | TCPDSTPORT ASSIGN field_value 
      { Mod(TCPDstPort (VInt.get_int16 (VInt.Int64 $3))) }
  | ID
      { id }
  | DROP 
      { drop }
  | INT64 AT INT64 DBLARROW INT64 AT INT64
      { Link($1, VInt.Int64 $3, $5, VInt.Int64 $7) }
  | LPAREN policy RPAREN 
      { $2 }

program : 
  | policy EOF  { $1 }

%%
