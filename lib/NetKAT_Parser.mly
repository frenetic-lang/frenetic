%{

  open NetKAT_Types





%}


%token LPAREN
%token RPAREN
%token BEGIN
%token END
%token LCURLY
%token RCURLY
%token NOT
/* XXX : Unused
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



/* TODO : Check, the precedence is different than NetCore, but it matches the paper */
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
  | SWITCH        { NetKAT_Types.Switch }
  | INPORT        { NetKAT_Types.Header.InPort }
  | SRCMAC        {}
  | DSTMAC        {}
  | VLAN          {}
  | SRCIP         {}
  | DSTIP         {}
  | TCPSRCPORT    {}
  | TCPDSTPORT    {}
  | FRAMETYPE     {}
  | PROTOCOLTYPE  {}

  
  
field_value :
  | INT64     {}
  | NONE      {}
  | MACADDR   {}
  | IPADDR    {}
/* esp for pred */
  | ARP {}
  | IP {}
  | ICMP {}
  | TCP  {}
  | UDP  {}




predicate :
  | LPAREN predicate RPAREN { $2 }
  | NOT predicate {}
  | STAR {}
  | NONE {}
  | field EQUALS field_value {}
  | predicate AND predicate {}
  | predicate OR predicate {}

/* In paper 
  | ID
  | DROP
*/

policy : 
  | FILTER predicate         {}
  | field field_value ASSIGN field_value {}
  | policy PLUS policy {}
  | policy SEMI policy {}
  | policy KLEEN_STAR  { NetKAT_Types.Star $1 }

  | PASS                     {}
  | DROP                     {}
  | LPAREN policy RPAREN     { $2 }
  | BEGIN  policy END        { $2 }
  | ALL                      {}
  | LCURLY predicate RCURLY policy LCURLY predicate RCURLY {}

/* TODO : Still possible for mismatched no of else as compared to original grammar */
  | IF predicate THEN policy {}
  | IF predicate THEN policy ELSE policy {}
  | LET ID EQUALS policy IN policy %prec IN {}

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
  | FWD LPAREN cexp RPAREN
  | FWD LPAREN cexp RPAREN AT INT64
*/


program : 
  | policy EOF  {}

%%
