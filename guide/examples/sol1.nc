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

let sol = 
  if frameType = arp then all
  else routing_for_103 + routing_for_104