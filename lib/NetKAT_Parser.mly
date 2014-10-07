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
  let vlan_none : int32 = Int32.minus_one
%}

%token LPAREN RPAREN BEGIN END LCURLY RCURLY
%token DBLARROW
%token NOT QMARK
%token AND OR
%token TRUE FALSE
%token PIPE QUERY
%token ALL FWD STAR
%token NONE
%token EQUALS
%token SWITCH PORT SRCMAC DSTMAC FRAMETYPE VLAN VLANPCP SRCIP DSTIP PROTOCOLTYPE TCPSRCPORT TCPDSTPORT
%token IF THEN ELSE
%token SEMI AMP BAR PLUS COMMA SLASH
%token LET IN
%token FILTER
%token ASSIGN
%token AT
%token ID DROP
%token MONITOR_POL MONITOR_TBL MONITOR_LOAD MONITOR_PKTS
%token <Int64.t> INT64
%token <Int64.t> INT48
%token <Int32.t> INT32
%token <Int32.t> INT16
%token <Int32.t> INT8
%token <Int64.t> MACADDR
%token <Int32.t> IPADDR
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token EOF

%start program

%type <NetKAT_Types.policy> program

%%

int16_value :
  | INT8
      { $1 }
  | INT16
      { $1 }

int32_value :
  | int16_value
      { $1 }
  | INT32
      { $1 }

int48_value :
  | int32_value
      { Int64.of_int32 $1 }
  | INT48
      { $1 }

int64_value :
  | int48_value
      { $1 }
  | INT64
      { $1 }


port_value :
  | int32_value
      { Location(Physical $1) }
  | PIPE LPAREN IDENT RPAREN
      { Location(Pipe $3) }
  | QUERY LPAREN IDENT RPAREN
      { Location(Query $3) }

mac_value :
  | MACADDR
      { $1 }
  | int48_value
      { $1 }

vlan_value :
  | INT16
      { Int32.to_int $1 }
  | NONE
      { Int32.to_int vlan_none }

ip_value :
  | IPADDR
      { $1 }
  | int32_value
      { $1 }

tcp_port_value :
  | int16_value
      { Int32.to_int $1 }

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
  | SWITCH EQUALS int64_value
      { Test(Switch $3) }
  | PORT EQUALS port_value
      { Test($3) }
  | SRCMAC EQUALS mac_value
      { Test(EthSrc $3) }
  | DSTMAC EQUALS mac_value
      { Test(EthDst $3) }
  | FRAMETYPE EQUALS int16_value
      { Test(EthType (Int32.to_int $3)) }
  | VLAN EQUALS vlan_value
      { Test(Vlan $3) }
  | VLANPCP EQUALS INT8
      { Test(VlanPcp (Int32.to_int $3)) }
  | SRCIP EQUALS ip_value
      { Test(IP4Src($3, 32l)) }
  | DSTIP EQUALS ip_value
      { Test(IP4Dst($3, 32l)) }
  | SRCIP EQUALS ip_value SLASH int32_value
      { Test(IP4Src($3, $5)) }
  | DSTIP EQUALS ip_value SLASH int32_value
      { Test(IP4Dst($3, $5)) }
  | PROTOCOLTYPE EQUALS int16_value
      { Test(IPProto (Int32.to_int $3)) }
  | TCPSRCPORT EQUALS tcp_port_value
      { Test(TCPSrcPort $3) }
  | TCPDSTPORT EQUALS tcp_port_value
      { Test(TCPDstPort $3) }

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
  | PORT ASSIGN port_value
      { Mod($3) }
  | SRCMAC ASSIGN mac_value
      { Mod(EthSrc $3) }
  | DSTMAC ASSIGN mac_value 
      { Mod(EthDst $3) }
  | FRAMETYPE ASSIGN int16_value
      { Mod(EthType (Int32.to_int $3)) }
  | VLAN ASSIGN vlan_value
      { Mod(Vlan $3) }
  | VLANPCP ASSIGN INT8
      { Mod(VlanPcp (Int32.to_int $3)) }
  | SRCIP ASSIGN ip_value
      { Mod(IP4Src($3, 32l)) }
  | DSTIP ASSIGN ip_value
      { Mod(IP4Dst($3, 32l)) }
  | PROTOCOLTYPE ASSIGN int16_value
      { Mod(IPProto (Int32.to_int $3)) }
  | TCPSRCPORT ASSIGN tcp_port_value
      { Mod(TCPSrcPort $3) }
  | TCPDSTPORT ASSIGN tcp_port_value
      { Mod(TCPDstPort $3) }
  | ID
      { id }
  | DROP 
      { drop }
  | int64_value AT int32_value DBLARROW int64_value AT int32_value
      { Link($1, $3, $5, $7) }
  | LPAREN policy RPAREN 
      { $2 }

program : 
  | policy EOF  { $1 }

%%
