%{
  open Types

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
%token EOF

%start program

%type <Types.policy> program

%%

field :
  | SWITCH 
      { Switch }
  | PORT 
      { Header SDN_Types.InPort }
  | SRCMAC
      { Header SDN_Types.EthSrc }
  | DSTMAC
      { Header SDN_Types.EthDst }
  | FRAMETYPE
      { Header SDN_Types.EthType }
  | VLAN
      { Header SDN_Types.Vlan }
  | VLANPCP
      { Header SDN_Types.VlanPcp }
  | SRCIP
      { Header SDN_Types.IP4Src }
  | DSTIP 
      { Header SDN_Types.IP4Dst }
  | PROTOCOLTYPE 
      { Header SDN_Types.IPProto }
  | TCPSRCPORT 
      { Header SDN_Types.TCPSrcPort }
  | TCPDSTPORT 
      { Header SDN_Types.TCPDstPort }

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
  | field EQUALS field_value 
      { Test ($1, VInt.Int64 $3) }

/* TODO(jnf): should these be non-associative? */
policy : 
  | policy PLUS spolicy
      { Choice ($1, $3) }
  | policy BAR spolicy
      { Par ($1, $3) }
  | spolicy 
      { $1 }

spolicy:
  | spolicy SEMI cpolicy 
      { Seq ($1, $3) }
  | cpolicy 
      { $1 }

cpolicy:
  | IF predicate THEN spolicy ELSE cpolicy 
      { Par(Seq(Filter $2, $4), Seq(Filter(Neg $2), $6)) }
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
  | field ASSIGN field_value 
      { Mod ($1, VInt.Int64 $3) }
  | ID
      { id }
  | DROP 
      { drop }
  | INT64 AT INT64 DBLARROW INT64 AT INT64
      { Link(VInt.Int64 $1, VInt.Int64 $3, VInt.Int64 $5, VInt.Int64 $7) }
  | LPAREN policy RPAREN 
      { $2 }

program : 
  | policy EOF  { $1 }

%%
