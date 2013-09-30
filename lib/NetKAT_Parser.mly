%{

  open NetKAT_Types

  (* Ethernet frame types *)
  let arp : int64  = Int64.of_int 0x806
  let ip  : int64  = Int64.of_int 0x800

  (* Ip protocol types *)
  let icmp : int64 = Int64.of_int 0x01
  let tcp  : int64 = Int64.of_int 0x06
  let udp  : int64 = Int64.of_int 0x11

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
 TODO : Classification of fields has been done now, is it fine or do we need to defer it to typechecker stage?
*/

/* TODO : Interface warnings are coming due to directly using SDN_Types.field */
switch_field : SWITCH { Switch }

switch_value : INT64 { $1 }


port_field :
  | INPORT        { Header InPort     }
  | TCPSRCPORT    { Header TCPSrcPort }
  | TCPDSTPORT    { Header TCPDstPort }

port_value : INT64 { $1 }



mac_field :
  | SRCMAC { Header EthSrc }
  | DSTMAC { Header EthDst }


mac_value : MACADDR { $1 }

vlan_field: VLAN { Header Vlan }

vlan_value : 
/* XXX : Hack */
  | NONE    { vlan_none  }
  | INT64   { $1 }



ip_field :
  | SRCIP         { Header IP4Src     }
  | DSTIP         { Header IP4Dst     }

ip_value : IPADDR  { $1 }



ether_frame_field : | FRAMETYPE     { Header EthType    }

ether_frame_value :
  | ARP     { arp  }
  | IP      { ip   }
  | INT64   { $1 }


proto_frame_field :
  | PROTOCOLTYPE  { Header IPProto    }

proto_frame_value :
  | ICMP    { icmp }
  | TCP     { tcp  }
  | UDP     { udp  }
  | INT64   { $1 }

  
  
predicate_test :
  | proto_frame_field EQUALS proto_frame_value { Test ($1, $3) }
  | ether_frame_field EQUALS ether_frame_value { Test ($1, $3) }
  | ip_field          EQUALS ip_value          { Test ($1, $3) }
  | vlan_field        EQUALS vlan_value        { Test ($1, $3) }
  | mac_field         EQUALS mac_value         { Test ($1, $3) }
  | port_field        EQUALS port_value        { Test ($1, $3) }
  | switch_field      EQUALS switch_value      { Test ($1, $3) }



predicate :
  | LPAREN predicate RPAREN  { $2 }
  | NOT predicate            { Neg $2 }
  | STAR                     { True }
  | NONE                     { False }
  | predicate_test           { $1 }
  | predicate AND predicate  { And ($1, $3) }
  | predicate OR predicate   { Or  ($1, $3) }


  /* TODO : define a policy that is only an identifier */

policy : 
  | FILTER predicate         { Filter $2 }

/*
  | field field_value ASSIGN field_value
    {
      (* Filter packets by $1 and then apply Mod to packets that filter out *)
      Seq (Filter (Test ($1, $2)), Mod ($1, $4))
    }
*/
  | policy PLUS policy { Par ($1, $3) }
  | policy SEMI policy { Seq ($1, $3) }
  | policy KLEEN_STAR  { Star $1 }
  | PASS               { id   }
  | DROP               { drop }
  | LPAREN policy RPAREN { $2 }
  | BEGIN  policy END    { $2 }


  | FWD LPAREN INT64 RPAREN          { Mod (InPort, $3) }

  /* XXX : Last INT64 is ignored here, similar to NetCore Parser */
  | FWD LPAREN INT64 RPAREN AT INT64 { Mod (InPort, $3) }


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
      Par (Seq ((Filter $2), $4), drop)
    }

  | IF predicate THEN policy ELSE policy
    {
      (* ((Filter predicate);pol1) + ((Filter (Not predicate));pol2) *)
      Par (Seq ((Filter $2), $4),
                 Seq ((Filter (Neg $2)), $6))
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
