%{

 open NetCore_Types
 open NetCore_SurfaceSyntax
 module Action = NetCore_Action.Output

 let int8_of_int64 (n : Int64.t) : int =
   if Int64.compare n Int64.zero >= 0 && 
     Int64.compare n (Int64.of_int 0xFF) <= 0 then
     Int64.to_int n
   else
     raise Parsing.Parse_error

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

 let int32_of_int64 (n : Int64.t) : int32 =
   if Int64.compare n Int64.zero >= 0 && 
     Int64.compare n (Int64.of_int32 Int32.max_int) <= 0 then
     (Int32.of_int (Int64.to_int n))
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
 
 let mk_fw sw inside outside =
   NetCore_StatefulFirewall.make sw inside outside
 
%}

%token LPAREN
%token RPAREN
%token BEGIN
%token END
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
%token RARROW
%token FILTER
%token INCLUDE
%token CHECK
%token FW

%start program

/* environment -> policy */
%type <NetCore_SurfaceSyntax.top> program

%%

seconds :
  | FLOAT { $1 }
  | INT64 { float_of_int (int_of_int64 $1) }

pred_atom :
  | LPAREN pred RPAREN { $2 }
  /* TODO(arjun): Support infix NOT too */
  | NOT pred_atom { Pol.Not $2 } /* Most useful for writing "!( ... )" */
  | STAR { Pol.Everything }
  | NONE { Pol.Nothing }
  | SWITCH EQUALS INT64 { Pol.OnSwitch $3 }
  /* ARJUN: I do not want the lexer to distinguish integers of different sizes.
     (i.e., I do not want users to have to write suffixed integers such as
     0xbeefbeefbeefbeefL for large integers. So, I'm lexing everything to 
     Int64, then having checked-casts down to the right size. */
  | INPORT EQUALS INT64 
    { Pol.Hdr (inPort (Pol.Physical (int32_of_int64 $3))) }
  | SRCMAC EQUALS MACADDR { Pol.Hdr (dlSrc $3) }
  | DSTMAC EQUALS MACADDR { Pol.Hdr (dlDst $3) }
  | VLAN EQUALS NONE { Pol.Hdr (dlVlan None) }
  | VLAN EQUALS INT64 { Pol.Hdr (dlVlan (Some (int12_of_int64 $3))) }
  | SRCIP EQUALS IPADDR { Pol.Hdr (ipSrc $3) }
  | DSTIP EQUALS IPADDR { Pol.Hdr (ipDst $3) }
  | TCPSRCPORT EQUALS INT64 { Pol.Hdr (tcpSrcPort (int16_of_int64 $3)) }
  | TCPDSTPORT EQUALS INT64 { Pol.Hdr (tcpDstPort (int16_of_int64 $3)) }
  | FRAMETYPE EQUALS ARP
    { Pol.Hdr (dlTyp 0x806) }
  | FRAMETYPE EQUALS IP
    { Pol.Hdr (dlTyp 0x800) }
  | FRAMETYPE EQUALS INT64
    { Pol.Hdr (dlTyp (int16_of_int64 $3)) }
  | PROTOCOLTYPE EQUALS ICMP
    { Pol.Hdr (ipProto 0x01) }
  | PROTOCOLTYPE EQUALS TCP
    { Pol.Hdr (ipProto 0x06) }
  | PROTOCOLTYPE EQUALS UDP
    { Pol.Hdr (ipProto 0x11) }
  | PROTOCOLTYPE EQUALS INT64
    { Pol.Hdr (ipProto (int8_of_int64 $3)) }
    
cexp :
  | ID
    { Id (symbol_start_pos (), $1) }
  | INT64
    { Value (Const $1) }

pred_or :
  | pred_atom { $1 }
  | pred_atom OR pred_or { Pol.Or ($1, $3) }

pred_and :
  | pred_or { $1 }
  | pred_or AND pred_and { Pol.And ($1, $3) }

pred :
  | pred_and { $1 }

pol_atom :
  | LPAREN pol RPAREN 
    { $2 }
  | BEGIN pol END 
    { $2 }
  | FILTER pred 
    { Filter (symbol_start_pos (), $2) }
  | cexp
    { CExp $1 }
  | FWD LPAREN cexp RPAREN
    { Action1 (symbol_start_pos (), $3,
                fun i -> Action.forward (int32_of_int64 i)) }
  | FWD LPAREN cexp RPAREN AT INT64
    { Action1 (symbol_start_pos (), $3,
                (* XXX: Why is the last int64 ignored? *)
                fun i -> Action.forward (int32_of_int64 i)) }
  | PASS 
    { Action (symbol_start_pos (), Action.pass) }
  | DROP 
    { Action (symbol_start_pos (), Action.drop) }
  | VLAN NONE RARROW cexp
    { Action1 (symbol_start_pos (), $4,
                fun i -> Action.updateDlVlan None (Some (int12_of_int64 i))) }
  | VLAN cexp RARROW cexp
    { Action2 (symbol_start_pos (), $2, $4,
                fun i j -> Action.updateDlVlan
                    (Some (int12_of_int64 i)) (Some (int12_of_int64 j))) }
  | VLAN cexp RARROW NONE
    { Action1 (symbol_start_pos (), $2,
                fun i -> Action.updateDlVlan (Some (int12_of_int64 i)) None) }
  | VLAN NONE RARROW NONE
    { Action (symbol_start_pos (), Action.updateDlVlan None None) }
  | TCPSRCPORT cexp RARROW cexp
    { Action2 (symbol_start_pos (), $2, $4,
                fun i j ->
                  Action.updateSrcPort (int16_of_int64 i) (int16_of_int64 j)) }
  | TCPDSTPORT cexp RARROW cexp
    { Action2 (symbol_start_pos (), $2, $4,
                fun i j ->
                  Action.updateDstPort (int16_of_int64 i) (int16_of_int64 j)) }
  | SRCMAC MACADDR RARROW MACADDR
    { Action (symbol_start_pos (), Action.updateDlSrc $2 $4) }
  | DSTMAC MACADDR RARROW MACADDR
    { Action (symbol_start_pos (), Action.updateDlDst $2 $4) }
  | SRCIP IPADDR RARROW IPADDR
    { Action (symbol_start_pos (), Action.updateSrcIP $2 $4) }
  | DSTIP IPADDR RARROW IPADDR
    { Action (symbol_start_pos (), Action.updateDstIP $2 $4) }
  | ALL 
    { Action (symbol_start_pos (), Action.to_all) }
  | MONITOR_POL LPAREN pol RPAREN
    { Transform (symbol_start_pos (), NetCore_Monitoring.monitor_pol, $3) }
  | MONITOR_TBL LPAREN INT64 COMMA pol RPAREN
    { Transform (symbol_start_pos (), NetCore_Monitoring.monitor_tbl $3, $5) }
  | MONITOR_LOAD LPAREN seconds COMMA STRING RPAREN
    { Action (symbol_start_pos (), NetCore_Monitoring.monitor_load $3 $5) }
  | MONITOR_PKTS LPAREN STRING RPAREN
    { Action (symbol_start_pos (), NetCore_Monitoring.monitor_packets $3) }
  | LCURLY pred RCURLY pol LCURLY pred RCURLY
    { Slice (symbol_start_pos (), $2, $4, $6) }
  | FW LPAREN INT64 COMMA INT64 COMMA INT64 RPAREN
    { let (lwt_e, pol) = mk_fw $3 (int32_of_int64 $5) (int32_of_int64 $7) in
      CExp (Value (PolStream (lwt_e, pol)))
    }

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
  | pol_pred_single PLUS pol_par_list
    { Par (symbol_start_pos (), $1, $3) }

pol :
  | pol_pred_single
    { $1 }
  | pol_pred_single PLUS pol_par_list
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
             [($2, CExp (Value (PolStream (lwt_e, priv))));
              ($4, CExp (Value (PolStream (Lwt.return (), pub))))],
             $13) }

program :
 | pol EOF 
    { Main (symbol_start_pos (), $1) }

 | LET ID EQUALS pol EOF
    { Bind (symbol_start_pos (), $2, $4,
        Main (symbol_start_pos (),
              Action (symbol_start_pos (), Action.drop))) }

 | LET ID EQUALS pol program 
    { Bind (symbol_start_pos (), $2, $4,  $5) }

 | INCLUDE STRING program
   { Include (symbol_start_pos (), $2, $3) }

 | CHECK STRING program
   { Check (symbol_start_pos (), $2, $3) }

%%
