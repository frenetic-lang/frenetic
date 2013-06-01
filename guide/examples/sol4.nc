let arpify(P:policy) =
  if frameType = arp then all
  else P

let deliver(s:switch, i:ip, p:port) =
  if switch = s && dstIP = i then fwd(p) 

let routing_for_103 = 
  deliver(103, 10.0.0.10, 2) + 
  deliver(103, 10.0.0.20, 3)

let routing_for_104 =
  deliver(104, 10.0.0.30, 2) +
  deliver(104, 10.0.0.40, 3)

let sol3 = 
  arpify(routing_for_103 + routing_for_104)

let monitor =
  if !(tcpDstPort = 80 || tcpDstPort = 22 || frameType = arp) then monitor_sw() 

let sol =
  sol3 + monitor 