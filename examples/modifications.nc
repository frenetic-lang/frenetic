(* This example demonstrates packet modifications.  Each of id* performs a
   sequence of modifications that ultimately yields an output packet equal to
   the input packet.  The final policy monitors packets before and after this
   sequence is applied; both BEFORE and AFTER packets should be identical.

   $ sudo mn --controller=remote --mac
   mininet> h1 ping -c 1 h2
*)

let id1 = 
  if srcMAC = 00:00:00:00:00:01 then
    (srcMAC 00:00:00:00:00:01 -> 00:00:00:00:00:10; srcMAC 00:00:00:00:00:10 -> 00:00:00:00:00:01)
  else
    pass in
let id2 = 
  if dstMAC = 00:00:00:00:00:02 then
    (dstMAC 00:00:00:00:00:02 -> 00:00:00:00:00:20; dstMAC 00:00:00:00:00:20 -> 00:00:00:00:00:02)
  else
    pass in
let id3 = vlan <none> -> 1; vlan 1 -> 2; vlan 2 -> <none> in
let id4 = 
  if srcIP = 10.0.0.1 then
    (srcIP 10.0.0.1 -> 10.1.1.1; srcIP 10.1.1.1 -> 10.0.0.1)
  else
    pass in
let id5 = 
  if dstIP = 10.0.0.2 then
    (dstIP 10.0.0.2 -> 10.1.1.2; dstIP 10.1.1.2 -> 10.0.0.2)
  else
    pass in
let id6 =
  if tcpSrcPort = 22 then
    (tcpSrcPort 22 -> 2222; tcpSrcPort 2222 -> 22)
  else
    pass in
let id7 = 
  if tcpDstPort = 22 then
    (tcpDstPort 22 -> 2222; tcpDstPort 2222 -> 22)
  else
    pass in
(monitorPackets("BEFORE") + pass); id1; id2; id3; id4; id5; id6; id7; (monitorPackets(" AFTER") + all)
