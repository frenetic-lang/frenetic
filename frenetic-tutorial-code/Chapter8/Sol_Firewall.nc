(* Copied from Sol_Chapter7_Firewall.nc, then s/forwarding/pass/g *)
let firewall = 
  if (tcpDstPort = 80 || tcpSrcPort = 80) &&
      ((dlSrc = 00:00:00:00:00:01 && dlDst = 00:00:00:00:00:02) ||
       (dlSrc = 00:00:00:00:00:02 && dlDst = 00:00:00:00:00:01))
  then
    pass
  else if nwProto = 1 && 
          ((dlSrc = 00:00:00:00:00:03 && dlDst = 00:00:00:00:00:04) ||
           (dlSrc = 00:00:00:00:00:04 && dlDst = 00:00:00:00:00:03))
       then
	 pass
       else
         drop