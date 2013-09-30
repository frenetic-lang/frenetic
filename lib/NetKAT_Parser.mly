%{

  open NetKAT_Types

  (* Ethernet frame types *)
  let arp  = 0x806
  let ip   = 0x800

  (* Ip protocol types *)
  let icmp = 0x01
  let tcp  = 0x06
  let udp  = 0x11

%}


%token LPAREN
%token RPAREN
%token BEGIN
%token END
%token LCURLY
%token RCURLY
%token NOT
/* XXX : Unused as of now
%token QMARK
%token TRUE
%token FALSE
*/
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
%token <Int32.t> IPADDR
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
/* TODO : 
%token MONITOR_POL
%token MONITOR_TBL
%token MONITOR_LOAD
%token MONITOR_PKTS
%token <string> STRING
*/
%token EOF
%token TICKTICKTICK /* only for Markdown */
%token ASSIGN
%token FILTER
%token INCLUDE
%token CHECK
%token FW
/* TODO : Come up with a symbol and define syntax and put in lexer */
%token KLEEN_STAR


/* To disambiguate let x = p1 in p2;x;p3 */
%nonassoc IN

%right THEN ELSE

/* kleen star binds tighter than semi, which binds tighter than plus */
%left PLUS
%left SEMI
%left KLEEN_STAR



/* TODO : Check, the precedence is different than NetCore, but it matches the NetKAT paper and boolean algebra */
%left OR
%left AND
%nonassoc NOT


%start program


%type <NetKAT_Types.policy> program

%%

/*
 TODO : "field" can be further classified into "switch", "layer2" "layer3" which can match to better 'typechecked' rules. But it has been left for typechecker to verify. Is this correct?
*/

field :
  | SWITCH        { NetKAT_Types.header.Switch            }
  | INPORT        { NetKAT_Types.header.Header.InPort     }
  | SRCMAC        { NetKAT_Types.header.Header.EthSrc     }
  | DSTMAC        { NetKAT_Types.header.Header.EthDst     }
  | VLAN          { NetKAT_Types.header.Header.Vlan       }
  | SRCIP         { NetKAT_Types.header.Header.IP4Src     }
  | DSTIP         { NetKAT_Types.header.Header.IP4Dst     }
  | TCPSRCPORT    { NetKAT_Types.header.Header.TCPSrcPort }
  | TCPDSTPORT    { NetKAT_Types.header.Header.TCPDstPort }
  | FRAMETYPE     { NetKAT_Types.header.Header.EthType    }
  | PROTOCOLTYPE  { NetKAT_Types.header.Header.IPProto    }

  
  
field_value :
  | INT64   { $1 }
  | NONE    { None  }
  | MACADDR { $1 }
  | IPADDR  { $1 }
  | ARP     { arp  }
  | IP      { ip   }
  | ICMP    { icmp }
  | TCP     { tcp  }
  | UDP     { udp  }


predicate :
  | LPAREN predicate RPAREN  { $2 }
  | NOT predicate            { NetKAT_Types.pred.Neg $2 }
  | STAR                     { pred.True }
  | NONE                     { pred.False }
  | field EQUALS field_value { NetKAT_Types.pred.Test ($1, $3) }
  | predicate AND predicate  { NetKAT_Types.pred.And  ($1, $3) }
  | predicate OR predicate   { NetKAT_Types.pred.Or   ($1, $3) }


  /* TODO : define a policy that is only an identifier */

policy : 
  | FILTER predicate         { NetKAT_Types.policy.Filter $2 }

  | field field_value ASSIGN field_value
    {
      (* Filter packets by $1 and then apply Mod to packets that filter out *)
      policy.Seq (policy.Filter (pred.Test ($1, $2)), policy.Mod ($1, $4))
    }


  | policy PLUS policy { NetKAT_Types.policy.Par ($1, $3) }
  | policy SEMI policy { NetKAT_Types.policy.Seq ($1, $3) }
  | policy KLEEN_STAR  { NetKAT_Types.Star $1 }
  | PASS               { NetKAT_Types.id   }
  | DROP               { NetKAT_Types.drop }
  | LPAREN policy RPAREN { $2 }
  | BEGIN  policy END    { $2 }


  | FWD LPAREN INT64 RPAREN          { policy.Mod (header.Header.InPort, $3) }

  /* XXX : Last INT64 is ignored here, similar to NetCore Parser */
  | FWD LPAREN INT64 RPAREN AT INT64 { policy.Mod (header.Header.InPort, $3) }


  /* TODO */
  | ALL
    {
      (* TODO : Forward to all ports in a switch except the one in which it arrived on *)
      (* forall ports in switch
           if (port != portin)
             dup pk, modify portout = port
      *)
    }

  | LCURLY predicate RCURLY policy LCURLY predicate RCURLY {}

/* TODO : Still possible for mismatched number of else as compared to original grammar */
  | IF predicate THEN policy 
    { 
      (* ((Filter predicate);policy) + drop *)
      policy.Par (policy.Seq ((policy.Filter $2), $4), drop)
    }

  | IF predicate THEN policy ELSE policy
    {
      (* ((Filter predicate);pol1) + ((Filter (Not predicate));pol2) *)
      policy.Par (policy.Seq ((policy.Filter $2), $4),
                  policy.Seq ((policy.Filter (pred.Neg $2)), $6))
    }

  | LET ID EQUALS policy IN policy %prec IN 
    {
      (* TODO  Evaluate in environment, maybe a simple substitution would do *)
      failwith "Not implemented yet"
      
    }

/* TODO :
  | LET ID COMMA ID EQUALS ID LPAREN PUBLICIP EQUALS IPADDR RPAREN IN pol
*/


/*
  TODO : 
  | MONITOR_POL LPAREN pol RPAREN
  | MONITOR_TBL LPAREN INT64 COMMA pol RPAREN
  | MONITOR_LOAD LPAREN seconds COMMA STRING RPAREN
  | MONITOR_PKTS LPAREN STRING RPAREN
*/
 

/* TODO : Dont Understand 
  | FW LPAREN INT64 COMMA INT64 COMMA INT64 RPAREN
  | cexp
*/


program : 
  | policy EOF  {}

%%
