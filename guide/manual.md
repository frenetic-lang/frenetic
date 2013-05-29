
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
<id> ::= [A-Z a-z _] [A-Z a-z _ 0-9]*

<seconds> ::= [0-9]+ | [0-9]+ . [0-9]+

<module> ::= learn ( )
           | nat ( publicIP = <ip-addr> )

<apol> ::= ( <pol> )
         | <id>
         | <port-id> (* haha, the syntax for forwarding, atrocious *)
         | pass
         | drop
         | all (* forward on all ports, not at all obvious *)
         | monitor_pol ( <pol> )
         | monitor_tbl ( <switch-id> , <pol> )
         | monitor_sw ( )
         | monitor_load (<seconds>, <pred>) (* Print the number of packets and bytes matching <pred> in the last <seconds>. *)

<cpol> ::= <apol>
        | if <pred> then <cpol> else <cpol>

<seq_pol_list> ::= <cpol>
                 | <cpol> ; <seq_pol_list>

<par_pol_list> ::= <cpol>
                 | <cpol> | <par_pol_list>

<pol> ::= <cpol>
        | <cpol> ; <seq_pol_list>
        | <cpol> | <par_pol_list>
        | let <id_1>, ... <id_n> = <module>(<arg_1> ,... , <arg_m>)

<program> ::= <pol>
```
