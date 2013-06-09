(* We'll use tshark to monitor email *)

let topo = 
  if dstMAC = ::1 then fwd(1)
  else if dstMAC = ::2 then fwd(2)
  else if dstMAC = ::3 then fwd(3)
  else if dstMAC = ::4 then fwd(4)
  else drop

let monitor =
  if tcpDstPort = 25 then
    fwd(666)
  else
    drop

let prog =
  monitorTable(0, monitor + topo)

prog
