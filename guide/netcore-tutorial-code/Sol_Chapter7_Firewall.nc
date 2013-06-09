(* Copied from Sol_Chapter7_Routing.nc. *)
let routing =
  if dstMAC=00:00:00:00:00:01 then
     fwd(1)
  else if dstMAC=00:00:00:00:00:02 then
    fwd(2)
  else if dstMAC=00:00:00:00:00:03 then
    fwd(3)
  else if dstMAC=00:00:00:00:00:04 then
    fwd(4)
  else if dstMAC=ff:ff:ff:ff:ff:ff then
    all (*  allow broadcasts *)
  else
    drop

(* This is a very naive way to write this firewall. *)
let firewall_or_route =
  if frameType=0x806 ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:02 && 
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
     (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:02 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:01 &&
      tcpDstPort = 2) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:03 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:04 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:03 && 
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:04 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (dstMAC = 00:00:00:00:00:01 && srcMAC = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
     (dstMAC = 00:00:00:00:00:01 && srcMAC = 00:00:00:00:00:02 && 
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
     (dstMAC = 00:00:00:00:00:01 && srcMAC = 00:00:00:00:00:04 &&
      tcpSrcPort = 80) ||
     (dstMAC = 00:00:00:00:00:02 && srcMAC = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:02 && srcMAC = 00:00:00:00:00:02 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:02 && srcMAC = 00:00:00:00:00:04 &&
      tcpSrcPort = 80) ||
    (dstMAC = 00:00:00:00:00:03 && srcMAC = 00:00:00:00:00:01 &&
      tcpSrcPort = 2) ||
    (dstMAC = 00:00:00:00:00:03 && srcMAC = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:03 && srcMAC = 00:00:00:00:00:02 &&
      tcpSrcPort = 25) ||
    (dstMAC = 00:00:00:00:00:03 && srcMAC = 00:00:00:00:00:03 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:03 && srcMAC = 00:00:00:00:00:04 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:04 && srcMAC = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:04 && srcMAC = 00:00:00:00:00:02 &&
      tcpSrcPort = 25) ||
    (dstMAC = 00:00:00:00:00:04 && srcMAC = 00:00:00:00:00:03 && 
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dstMAC = 00:00:00:00:00:04 && srcMAC = 00:00:00:00:00:04 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25))


  then
    routing
  else
    drop

monitorTable(1, firewall_or_route)