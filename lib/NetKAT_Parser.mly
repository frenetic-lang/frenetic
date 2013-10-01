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
/* %token <Int32.t> IPADDR */
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



/* TODO : Check, the precedence here is different than NetCore, but it matches the NetKAT paper and boolean algebra */
%left OR
%left AND
%nonassoc NOT


%start program


%type <NetKAT_Types.policy> program

%%

/* TODO : Interface warnings are coming due to directly using SDN_Types.field */



/* XXX : The following parser for field and field_values gives an impression that any value can be assigned to any field. For example arp can be assigned to IP4Src since they have a similar type. The decision has been deferred to typechecker to check the correctness of integer values if valid integer width types have been assigned to corresponding fields
*/

field :
  | SWITCH       { Switch            }
  | INPORT       { Header InPort     }
  | TCPSRCPORT   { Header TCPSrcPort }
  | TCPDSTPORT   { Header TCPDstPort }
  | SRCMAC       { Header EthSrc     }
  | DSTMAC       { Header EthDst     }
  | VLAN         { Header Vlan       }
  | SRCIP        { Header IP4Src     }
  | DSTIP        { Header IP4Dst     }
  | FRAMETYPE    { Header EthType    }
  | PROTOCOLTYPE { Header IPProto    }


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
  | field EQUALS field_value { Test ($1, Int64 $3) }
  | predicate AND predicate  { And  ($1, $3)       }
  | predicate OR predicate   { Or   ($1, $3)       }




policy : 
  | FILTER predicate         { Filter $2 }

  /* TODO : define a policy that is only an identifier */
  /* | ID { $1 } */

  | field field_value ASSIGN field_value
    {
      (* Filter packets by $1 and then apply Mod to packets that filter out *)
      Seq (Filter (Test ($1, Int64 $2)), Mod ($1, Int64 $4))
    }

  | policy PLUS policy   { Par ($1, $3) }
  | policy SEMI policy   { Seq ($1, $3) }
  | policy KLEEN_STAR    { Star $1      }
  | PASS                 { id           }
  | DROP                 { drop         }
  | LPAREN policy RPAREN { $2           }
  | BEGIN  policy END    { $2           }


  | FWD LPAREN INT64 RPAREN          { Mod (Header InPort, Int64 $3) }

  /* XXX : Last INT64 is ignored here, similar to NetCore Parser */
  | FWD LPAREN INT64 RPAREN AT INT64 { Mod (Header InPort, Int64 $3) }


  /* TODO */
  | ALL
    {
      (* TODO : Forward to all ports in a switch except the one in which it arrived on *)
      (* forall ports in switch
           if (port != portin)
             dup pk, modify portout = port
      *)

      (* XXX : For now, for the sake of completeness of parser *)
      drop
    }

  /* Not there in the documentation */
  | LCURLY predicate RCURLY policy LCURLY predicate RCURLY { drop }

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
      drop
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
  | policy EOF  { $1 }

%%
