%{
(** A JavaScript parser that does not do semicolon insertion. *)

 open Mininet_Types

 type mnDest = 
   | ToHost of Int64.t 
   | ToSwitch of Int64.t * int

 let make_edge (sw : Int64.t) (pred_pt : int) dest = 
   match dest with
     | ToHost mac -> [(Host mac, 0, Switch sw); 
                      (Switch sw, pred_pt + 1, Host mac)]
     (* there will be symmetric entry for this switch *)
     | ToSwitch (dst, _) -> [(Switch sw, pred_pt + 1, Switch dst)]
 
%}

%token <Int64.t> SWITCH
%token <Int64.t> TOHOST
%token <Int64.t * int> TOSWITCH
%token LINKS

%token EOF

%start program

%type <(Mininet_Types.node * Mininet_Types.portId * Mininet_Types.node) list>
       program

%%

dest :
  | TOHOST { ToHost $1 }
  | TOSWITCH { match $1 with | (dpid,pt) -> ToSwitch (dpid, pt) }
                (* ^^^ too much coq *)

dests
  : dest { [$1] }
  | dest dests { $1 :: $2 }

(* Always ends in a newline, but the next token on the new line is
   distinct, so we don't have any ambiguity if there are extra/elided
   linebreaks. *)
switch : 
  | SWITCH LINKS dests { List.concat (List.mapi (make_edge $1) $3) }

switches
  : switch { $1 }
  | switch switches { $1 @ $2 }

program
  : switches EOF { $1 }

%%
