let monitoring =
  if dlSrc  = 00:00:00:00:00:01 && tcpSrcPort = 80 then 
    if dlDst = 00:00:00:00:00:01 then
      monitorLoad(5, "H2->H1")
    else if dlDst = 00:00:00:00:00:03 then
      monitorLoad(5, "H2->H3")
    else if dlDst = 00:00:00:00:00:04 then
      monitorLoad(5, "H2->H4")
    else
      drop