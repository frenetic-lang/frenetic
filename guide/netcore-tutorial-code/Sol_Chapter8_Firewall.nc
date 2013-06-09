include "Sol_Chapter8_FirewallModule.nc"

let routing =
  if dstMAC = ff:ff:ff:ff:ff:ff then
    all    
  else if switch = 1 then
    if inPort = 1 then
      fwd(2)
    else if inPort = 2 then
      fwd(1)
    else
      drop
  else if switch = 2 then
    if dstMAC = 00:00:00:00:00:01 then
      fwd(1)
    else if dstMAC = 00:00:00:00:00:02 then
      fwd(2)
    else
      fwd(3)
  else if switch = 3 then
    if dstMAC = 00:00:00:00:00:03 then
      fwd(1)
    else if dstMAC = 00:00:00:00:00:04 then
      fwd(2)
    else
      fwd(3)
  else
    drop

firewall; routing
