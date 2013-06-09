(* 
  You're running an unprivileged Web server on port 8080.
  Use SDN to expose it on port 80.

   Server on port 1 of the switch
   Outside world is port 2
*)

(* Using sequential composition below, but it a completely trivial use
   case. Should not make anyone's head explode *)
let nat_this_network =
  if inPort = 1 && tcpSrcPort = 8080 then
    (tcpSrcPort 8080 -> 80; fwd(2))
  else if inPort = 2 && tcpDstPort = 80 then
    (tcpDstPort 80 -> 8080; fwd(1))
  else
    drop


(* Step 2: your NAT is so useful! Congratulations! Now the topology
   changed (several public ports) and you need to rewrite your NAT. :( *)

let nat_private =
  if tcpSrcPort = 8080 then
    tcpSrcPort 8080 -> 80
  else
    pass

let nat_public =
  if tcpDstPort = 80 then
    tcpDstPort 80 -> 8080
  else
    pass

let nat =
  if inPort = 1 then nat_private
  else nat_public

let topo = 
  if dstMAC = ::1 then fwd(1)
  else if dstMAC = ::2 then fwd(2)
  else if dstMAC = ::3 then fwd(3)
  else if dstMAC = ::4 then fwd(4)
  else drop

let use_nat = monitorTable(0, nat; topo)

(* Step 3: unexpected hack (prolly not for tutorial) *)

(* "nat" still has "1" baked in as the private port and everything else
   as the public port. *)


(* This NAT module expects all private-traffic to be on port 9000
   and all public traffic to be on port 10000. It is identity on other ports
  *)
let nat2 =
  if inPort = 9000 && tcpSrcPort = 8080 then
    tcpSrcPort 8080 -> 80
  else if inPort = 10000 && tcpDstPort = 80 then
    tcpDstPort 80 -> 8080
  else
    pass

let use_nat2 =
  if inPort = 1 then 
    (fwd(9000);  (* this is private traffic *)
     nat2;
     topo)
  else
    (fwd(1000);  (* this is public traffic *)
     nat2;
     topo)

let use_nat2_monitor = monitorTable(0, use_nat2)

use_nat2_monitor
