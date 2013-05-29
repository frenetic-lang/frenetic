NetCore Syntax
==============

Types and stuff:

```
(* Integers can be either decimal or hexadecimal (with leading 0x *)

<mac-address> ::= xx:xx:xx:xx:xx:xx
<ip-address> ::= xxx.xxx.xxx.xxx
<switch-id> ::= 64-bit integer
<port-id> ::= 16-bit integer
<vlan-id> ::= none | 12-bit integer
<tcp-port> ::= 16-bit integer
<frame-type> ::= arp (* shorthand for 0x806 *)
               | ip  (* shorthand for 0x800 *)
               | 8-bit integer
```

Predicates:

```
<apred> ::= ( <pred> )
          | ! <apred> 
          | *
          | <none>
          | switch = <switch-id>
          | inPort = <port-id>
          | srcMac = <mac-address>
          | dstMac = <mac-address>
          | vlan = <vlan-id>
          | srcIP = <ip-address>
          | dstIP = <ip-address>
          | tcpSrcPort = <tcp-port>
          | tcpDstPort = <tcp-port>
          | frameType = <frame-type>

<orpred> ::= <apred>
           | <apred> || <orpred>

<pred> ::= <orpred>
         | <orpred> && <pred>

```

Policies:

```

pol_atom :
  | LPAREN pol RPAREN 
  | ID 
  | INT64 
  | PASS 
  | DROP 
  | ALL 
  | MONITOR_POL LPAREN pol RPAREN
  | MONITOR_TBL LPAREN INT64 COMMA pol RPAREN
  | MONITOR_SW LPAREN RPAREN 

pol_pred :  
  | pol_atom
  | IF pred THEN pol_pred ELSE pol_pred

pol_seq_list :
  | pol_pred 
  | pol_pred SEMI pol_seq_list 

pol_par_list :
  | pol_pred
  | pol_pred BAR pol_par_list

pol :
  | pol_pred 
  | pol_pred BAR pol_par_list
  | pol_pred SEMI pol_seq_list
  | LET ID EQUALS LEARNING IN pol
  | LET ID COMMA ID EQUALS NAT LPAREN PUBLICIP EQUALS IPADDR RPAREN IN pol

program
  : pol EOF { $1 }

```