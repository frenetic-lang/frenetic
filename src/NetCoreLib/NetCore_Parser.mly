%{

 open NetCore_SurfaceSyntax
 module Pat = NetCore_Pattern
 module Action = NetCore_Action.Output

 let int12_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 && 
     Int64.compare n (Int64.of_int 0xFFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let int16_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 && 
     Int64.compare n (Int64.of_int 0xFFFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let int_of_int64 (n : Int64.t) : int =
   if Int64.compare n (Int64.of_int max_int) <= 0 &&
     Int64.compare n (Int64.of_int min_int) >= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

 let nat_hash = Hashtbl.create 7

 let mk_nat ip = 
   (if not (Hashtbl.mem nat_hash ip) then 
       Hashtbl.add nat_hash ip (NetCore_NAT.make ip));
   Hashtbl.find nat_hash ip
  
%}

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token NOT
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
%token <string> ID
%token COMMA
%token LET
%token IN
%token PUBLICIP
%token PASS
%token DROP
%token FRAMETYPE
%token ARP
%token IP
%token MONITOR_POL
%token MONITOR_TBL
%token MONITOR_SW
%token MONITOR_LOAD
%token <string> STRING
%token EOF
%token TICKTICKTICK /* only for Markdown */
%token RARROW
%token FILTER

%start program

/* environment -> policy */
%type <NetCore_SurfaceSyntax.exp> program

%%

seconds :
  | FLOAT { $1 }
  | INT64 { float_of_int (int_of_int64 $1) }

pred_atom :
  | LPAREN pred RPAREN { $2 }
  /* TODO(arjun): Support infix NOT too */
  | NOT pred_atom { Pol.PrNot $2 } /* Most useful for writing "!( ... )" */
  | STAR { Pol.PrAll }
  | NONE { Pol.PrNone }
  | SWITCH EQUALS INT64 { Pol.PrOnSwitch $3 }
  /* ARJUN: I do not want the lexer to distinguish integers of different sizes.
     (i.e., I do not want users to have to write suffixed integers such as
     0xbeefbeefbeefbeefL for large integers. So, I'm lexing everything to 
     Int64, then having checked-casts down to the right size. */
  | INPORT EQUALS INT64 
    { Pol.PrHdr (Pat.inPort (Pol.Physical (int16_of_int64 $3))) }
  | SRCMAC EQUALS MACADDR { Pol.PrHdr (Pat.dlSrc $3) }
  | DSTMAC EQUALS MACADDR { Pol.PrHdr (Pat.dlDst $3) }
  | VLAN EQUALS NONE { Pol.PrHdr (Pat.dlVlan None) }
  | VLAN EQUALS INT64 { Pol.PrHdr (Pat.dlVlan (Some (int12_of_int64 $3))) }
  | SRCIP EQUALS IPADDR { Pol.PrHdr (Pat.ipSrc $3) }
  | DSTIP EQUALS IPADDR { Pol.PrHdr (Pat.ipDst $3) }
  | TCPSRCPORT EQUALS INT64 { Pol.PrHdr (Pat.tcpSrcPort (int16_of_int64 $3)) }
  | TCPDSTPORT EQUALS INT64 { Pol.PrHdr (Pat.tcpDstPort (int16_of_int64 $3)) }
  | FRAMETYPE EQUALS ARP
    { Pol.PrHdr (Pat.dlType 0x806) }
  | FRAMETYPE EQUALS IP
    { Pol.PrHdr (Pat.dlType 0x800) }
  | FRAMETYPE EQUALS INT64
    { Pol.PrHdr (Pat.dlType (int16_of_int64 $3)) }
    

pred_or :
  | pred_atom { $1 }
  | pred_atom OR pred_or { Pol.PrOr ($1, $3) }

pred_and :
  | pred_or { $1 }
  | pred_or AND pred_and { Pol.PrAnd ($1, $3) }

pred :
  | pred_and { $1 }

pol_atom :
  | LPAREN pol RPAREN 
    { $2 }
  | FILTER pred { Filter (symbol_start_pos (), $2) }
  | ID 
    { Id (symbol_start_pos (), $1) }
  | FWD LPAREN INT64 RPAREN 
    { Action (symbol_start_pos (), Action.forward (int16_of_int64 $3)) }
  | PASS 
    { Action (symbol_start_pos (), Action.pass) }
  | DROP 
    { Action (symbol_start_pos (), Action.drop) }
  | VLAN NONE RARROW INT64
    { Action (symbol_start_pos (),
              Action.updateDlVlan None (Some (int12_of_int64 $4))) }
  | ALL 
    { Action (symbol_start_pos (), Action.to_all) }
  | MONITOR_POL LPAREN pol RPAREN
    { Transform (symbol_start_pos (), NetCore_Monitoring.monitor_pol, $3) }
  | MONITOR_TBL LPAREN INT64 COMMA pol RPAREN
    { Transform (symbol_start_pos (), NetCore_Monitoring.monitor_tbl $3, $5) }
  | MONITOR_SW LPAREN RPAREN 
    { HandleSwitchEvent
      (symbol_start_pos (), NetCore_Monitoring.monitor_switch_events) }
  | MONITOR_LOAD LPAREN seconds COMMA pred RPAREN
    { Transform 
      ( symbol_start_pos ()
      , NetCore_Monitoring.monitor_load $3
      , Filter (symbol_start_pos (), $5) ) }
  | LCURLY pred RCURLY pol LCURLY pred RCURLY
    { Slice (symbol_start_pos (), $2, $4, $6) }

pol_pred_double :  
  | pol_atom
    { $1 }
  | IF pred THEN pol_pred_double ELSE pol_pred_double
    { ITE (symbol_start_pos (), $2, $4, $6) }

pol_pred_single:
  | pol_pred_double
    { $1 }
  | IF pred THEN pol_pred_double 
    { ITE (symbol_start_pos (), $2, $4, Action (symbol_start_pos (), Action.drop)) }
pol_seq_list :
  | pol_pred_single 
    { $1 }
  | pol_pred_single SEMI pol_seq_list 
    { Seq (symbol_start_pos (), $1, $3) }

pol_par_list :
  | pol_pred_single
    { $1 }
  | pol_pred_single BAR pol_par_list
    { Par (symbol_start_pos (), $1, $3) }

pol :
  | pol_pred_single 
    { $1 }
  | pol_pred_single BAR pol_par_list
    { Par (symbol_start_pos (), $1, $3) }
  | pol_pred_single SEMI pol_seq_list
    { Seq (symbol_start_pos (), $1, $3) }
  | LET ID EQUALS pol IN pol
    { Let (symbol_start_pos (),
           [($2, $4)],
           $6) }
  | LET ID COMMA ID EQUALS ID LPAREN PUBLICIP EQUALS IPADDR RPAREN IN pol
    { (* TODO(arjun): sort of strange, but correct. its the private policy
         that induces policy updates. *)
      if $6 <> "nat" then 
	raise Parsing.Parse_error
      else
	let (lwt_e, priv, pub) = mk_nat $10 in
	Let (symbol_start_pos (),
             [($2, Value (PolStream (lwt_e, priv)));
              ($4, Value (PolStream (Lwt.return (), pub)))],
             $13) }

program
  : pol EOF { $1 }

%%
