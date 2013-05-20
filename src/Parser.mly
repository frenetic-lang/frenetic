%{

 open Syntax.External

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
  
%}

%token LPAREN
%token RPAREN
%token NOT
%token ALL
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
%token AND
%token OR
%token IF
%token THEN
%token SEMI
%token BAR
%token EOF

%start program

%type <Syntax.External.policy> program

%%

pred_atom :
  | LPAREN pred RPAREN { $2 }
  (* TODO(arjun): Support infix NOT too *)
  | NOT pred_atom { Not $2 } (* Most useful for writing "!( ... )" *)
  | STAR { All }
  | NONE { NoPackets }
  | SWITCH EQUALS INT64 { Switch $3 }
  (* ARJUN: I do not want the lexer to distinguish integers of different sizes.
     (i.e., I do not want users to have to write suffixed integers such as
     0xbeefbeefbeefbeefL for large integers. So, I'm lexing everything to 
     Int64, then having checked-casts down to the right size. *)
  | INPORT EQUALS INT64 { InPort (int16_of_int64 $3) }
  | SRCMAC EQUALS MACADDR { DlSrc $3 }
  | DSTMAC EQUALS MACADDR { DlDst $3 }
  | VLAN EQUALS NONE { DlVlan None }
  | VLAN EQUALS INT64 { DlVlan (Some (int12_of_int64 $3)) }
  | SRCIP EQUALS IPADDR { SrcIP $3 }
  | DSTIP EQUALS IPADDR { DstIP $3 }
  | TCPSRCPORT EQUALS INT64 { TcpSrcPort (int16_of_int64 $3) }
  | TCPDSTPORT EQUALS INT64 { TcpDstPort (int16_of_int64 $3) }

pred_or :
  | pred_atom { $1 }
  | pred_atom OR pred_or { Or ($1, $3) }

pred_and :
  | pred_or { $1 }
  | pred_or AND pred_and { And ($1, $3) }

pred :
  | pred_and { $1 }


pol_atom :
  | LPAREN pol RPAREN { $2 }
  | INT64 { Act (To (int16_of_int64 $1)) }
  | ALL { Act ToAll }

pol_pred :  
  | pol_atom { $1 }
  | IF pred THEN pol_pred { Seq (Filter $2, $4) }

pol_seq_list :
  | pol_pred { $1 }
  | pol_pred SEMI pol_seq_list { Seq ($1, $3) }

pol_par_list :
  | pol_pred { $1 }
  | pol_pred BAR pol_par_list { Par ($1, $3) }

pol :
  | pol_pred { $1 }
  | pol_pred BAR pol_par_list { Par ($1,$3) }
  | pol_pred SEMI pol_seq_list { Seq ($1,$3) }


program
  : pol EOF { $1 }

%%
