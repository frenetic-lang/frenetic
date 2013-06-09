let routing_for_103 = 
  if switch = 103 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(2) + 
      if dstIP = 10.0.0.20 then fwd(3) 
    end

let routing_for_104 = 
  if switch = 104 then 
    begin 
      if dstIP = 10.0.0.30 then fwd(2) + 
      if dstIP = 10.0.0.40 then fwd(3) 
    end

let routing = 
  if frameType = arp then 
    all
  else 
    begin 
      routing_for_103 + 
      routing_for_104
    end

let monitoring = 
  (* JNF: Not sure what's intended here. The monitorSwitch policy
     operator tracks topology changes, so it makes little sense under
     a predicate... but faithfully reflecting the previous structure
     for now. *)
  if !(tcpDstPort = 80 || tcpDstPort = 22 || frameType = arp) then monitorSwitch 

let sol4 = routing + monitoring

sol4
