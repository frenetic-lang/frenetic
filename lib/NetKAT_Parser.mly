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


%token LPAREN
%token RPAREN
%token BEGIN
%token END
%token LCURLY
%token RCURLY
%token NOT
%token QMARK
%token TRUE
%token FALSE
%token ALL
%token FWD
%token STAR
%token NONE
%token EQUALS
%token SWITCH
%token VLAN
%token SRCMAC
%token DSTMAC
%token SRCIP
%token DSTIP
%token TCPSRCPORT
%token TCPDSTPORT
%token INPORT
%token <Int64.t> INT64
%token <Int64.t> MACADDR
%token <Int64.t> IPADDR
%token <float> FLOAT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token SEMI
%token BAR
%token PLUS
%token <string> ID
%token COMMA
%token LET
%token IN
%token AT
%token PUBLICIP
%token PASS
%token DROP
%token FRAMETYPE
%token PROTOCOLTYPE
%token ARP
%token IP
%token ICMP
%token TCP
%token UDP
%token MONITOR_POL
%token MONITOR_TBL
%token MONITOR_LOAD
%token MONITOR_PKTS
%token <string> STRING
%token EOF
%token TICKTICKTICK /* only for Markdown */
%token ASSIGN
%token FILTER
%token INCLUDE
%token CHECK
%token FW
/* TODO : Come up with a symbol and define syntax and put in lexer */
%token KLEEN_STAR


/* To disambiguate "let x = p1 in p2;x;p3", SEQ binds tighter than  */
%nonassoc IN

%right THEN ELSE

/* kleen star binds tighter than semi, which binds tighter than plus */
%left PLUS
%left SEMI
%left KLEEN_STAR

%left OR
%left AND
%nonassoc NOT


%start program


%type <NetKAT_Types.policy> program

%%

/* XXX : The following parser for field and field_values gives an impression that any value can be assigned to any field. For example arp can be assigned to IP4Src since they have a similar type. The decision has been deferred to typechecker to check the correctness of integer values if valid integer width types have been assigned to corresponding fields
*/

field :
  | SWITCH       { Switch            }
  | INPORT       { Header SDN_Types.InPort     }
  | TCPSRCPORT   { Header SDN_Types.TCPSrcPort }
  | TCPDSTPORT   { Header SDN_Types.TCPDstPort }
  | SRCMAC       { Header SDN_Types.EthSrc     }
  | DSTMAC       { Header SDN_Types.EthDst     }
  | VLAN         { Header SDN_Types.Vlan       }
  | SRCIP        { Header SDN_Types.IP4Src     }
  | DSTIP        { Header SDN_Types.IP4Dst     }
  | FRAMETYPE    { Header SDN_Types.EthType    }
  | PROTOCOLTYPE { Header SDN_Types.IPProto    }


field_value :
  | INT64   { $1        }
  | MACADDR { $1        }
  | NONE    { vlan_none }
  | IPADDR  { $1        }
  | ARP     { arp       }
  | IP      { ip        }
  | ICMP    { icmp      }
  | TCP     { tcp       }
  | UDP     { udp       }

  
predicate :
  | LPAREN predicate RPAREN  { $2                  }
  | NOT predicate            { Neg $2              }
  | STAR                     { True                }
  | NONE                     { False               }
  | field EQUALS field_value { Test ($1, VInt.Int64 $3) }
  | predicate AND predicate  { And  ($1, $3)       }
  | predicate OR predicate   { Or   ($1, $3)       }

policy : 
  | FILTER predicate         { Filter $2 }
  | field ASSIGN field_value { Mod ($1, VInt.Int64 $3) }
  | policy PLUS policy   { Par ($1, $3) }
  | policy SEMI policy   { Seq ($1, $3) }
  | policy KLEEN_STAR    { Star $1      }
  | PASS                 { id           }
  | DROP                 { drop         }
  | LPAREN policy RPAREN { $2           }


program : 
  | policy EOF  { $1 }

%%
