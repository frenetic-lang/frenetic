let forwarding =
  if dlDst=00:00:00:00:00:01 then
    fwd(1)
  else if dlDst=00:00:00:00:00:02 then
    fwd(2)
  else if dlDst=00:00:00:00:00:03 then
    fwd(3)
  else if dlDst=00:00:00:00:00:04 then
    fwd(4)
  else
    drop
    
monitorTable(1, forwarding)
