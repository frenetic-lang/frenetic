(* Copied from Sol_Chapter7_Firewall.nc, then s/routing/pass/g *)
let firewall = 
  if dlTyp=0x806 ||
     (dlSrc = 00:00:00:00:00:01 && dlDst = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (dlSrc = 00:00:00:00:00:01 && dlDst = 00:00:00:00:00:02 && 
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (dlSrc = 00:00:00:00:00:01 && dlDst = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
     (dlSrc = 00:00:00:00:00:02 && dlDst = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:02 && dlDst = 00:00:00:00:00:02 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:02 && dlDst = 00:00:00:00:00:04 &&
      tcpDstPort = 80) ||
    (dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:01 &&
      tcpDstPort = 2) ||
    (dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:03 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:04 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:04 && dlDst = 00:00:00:00:00:01 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:04 && dlDst = 00:00:00:00:00:02 &&
      tcpDstPort = 25) ||
    (dlSrc = 00:00:00:00:00:04 && dlDst = 00:00:00:00:00:03 && 
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
    (dlSrc = 00:00:00:00:00:04 && dlDst = 00:00:00:00:00:04 &&
      (tcpDstPort = 80 || tcpDstPort = 25)) ||
     (dlDst = 00:00:00:00:00:01 && dlSrc = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
     (dlDst = 00:00:00:00:00:01 && dlSrc = 00:00:00:00:00:02 && 
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
     (dlDst = 00:00:00:00:00:01 && dlSrc = 00:00:00:00:00:04 &&
      tcpSrcPort = 80) ||
     (dlDst = 00:00:00:00:00:02 && dlSrc = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:02 && dlSrc = 00:00:00:00:00:02 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:02 && dlSrc = 00:00:00:00:00:04 &&
      tcpSrcPort = 80) ||
    (dlDst = 00:00:00:00:00:03 && dlSrc = 00:00:00:00:00:01 &&
      tcpSrcPort = 2) ||
    (dlDst = 00:00:00:00:00:03 && dlSrc = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:03 && dlSrc = 00:00:00:00:00:02 &&
      tcpSrcPort = 25) ||
    (dlDst = 00:00:00:00:00:03 && dlSrc = 00:00:00:00:00:03 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:03 && dlSrc = 00:00:00:00:00:04 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:04 && dlSrc = 00:00:00:00:00:01 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:04 && dlSrc = 00:00:00:00:00:02 &&
      tcpSrcPort = 25) ||
    (dlDst = 00:00:00:00:00:04 && dlSrc = 00:00:00:00:00:03 && 
      (tcpSrcPort = 80 || tcpSrcPort = 25)) ||
    (dlDst = 00:00:00:00:00:04 && dlSrc = 00:00:00:00:00:04 &&
      (tcpSrcPort = 80 || tcpSrcPort = 25))
  then
    pass
  else
    drop
