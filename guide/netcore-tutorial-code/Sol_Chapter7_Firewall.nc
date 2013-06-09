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

let firewall_or_route =
  if frameType=0x806 ||
     (tcpSrcPort = 80 || tcpSrcPort = 22 || tcpSrcPort = 25) ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:02 && 
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = 00:00:00:00:00:01 && dstMAC = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
     (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:02 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:02 && dstMAC = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:01 &&
      tcpDstPort = 2) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:03 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:03 && dstMAC = 00:00:00:00:00:04 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:01 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:03 && 
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = 00:00:00:00:00:04 && dstMAC = 00:00:00:00:00:04 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25))
  then
    routing
  else
    drop

monitorTable(1, firewall_or_route)