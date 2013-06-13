let routing_for_102 =
  if switch = 102 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(2) + 
      if dstIP = 10.0.0.20 then fwd(2) + 
      if dstIP = 10.0.0.30 then fwd(3) + 
      if dstIP = 10.0.0.40 then fwd(3)
    end

let routing_for_103 =
  if switch = 103 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(2) + 
      if dstIP = 10.0.0.20 then fwd(3) + 
      if dstIP = 10.0.0.30 then fwd(1) + 
      if dstIP = 10.0.0.40 then fwd(1)
    end


let routing_for_104 =
  if switch = 104 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(1) + 
      if dstIP = 10.0.0.20 then fwd(1) + 
      if dstIP = 10.0.0.30 then fwd(2) + 
      if dstIP = 10.0.0.40 then fwd(3)
    end

let sol2 =
  if dlTyp = arp then 
    all 
  else 
    begin 
      routing_for_102 + 
      routing_for_103 + 
      routing_for_104
    end

sol2
