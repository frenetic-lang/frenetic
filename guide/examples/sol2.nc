let arpify(P:policy) =
  if frameType = arp then all
  else P

let deliver(s:switch, i:ip, p:port) =
  if switch = s && dstip = i then fwd(p) 

let routing_for_103 =
  deliver(103, 10.0.0.10, 2) + 
  deliver(103, 10.0.0.20, 3) +
  deliver(103, 10.0.0.30, 1) +
  deliver(103, 10.0.0.40, 1)

let routing_for_102 =
  deliver(102, 10.0.0.10, 2) + 
  deliver(102, 10.0.0.20, 2) +
  deliver(102, 10.0.0.30, 3) +
  deliver(102, 10.0.0.40, 3)

let routing_for_104 =
  deliver(104, 10.0.0.10, 1) + 
  deliver(104, 10.0.0.20, 1) +
  deliver(104, 10.0.0.30, 2) +
  deliver(104, 10.0.0.40, 3)

let sol2 =
  arpify(routing_for_102 + routing_for_103 + routing_for_104)