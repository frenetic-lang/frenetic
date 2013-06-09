(* Copied from Sol_Chapter7_Firewall.nc, then s/routing/pass/g *)
let firewall = 
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
    pass
  else
    drop
