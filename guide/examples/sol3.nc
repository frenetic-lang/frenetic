(* JNF: What is this example teaching? It's extremely similar to
sol2.nc, just more complicated... *)

let routing_for_101 =
  if switch = 101 then
    begin 
      if srcIP = 10.0.0.5 then fwd(1)
      else fwd(2)
    end

let routing_for_102 =
  if switch = 102 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(2) + 
      if dstIP = 10.0.0.20 then fwd(2) + 
      if dstIP = 10.0.0.30 then fwd(3) + 
      if dstIP = 10.0.0.40 then fwd(3) + 
      if dstIP = 10.0.0.50 then fwd(1)
    end

let routing_for_103 =
  if switch = 103 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(2) + 
      if dstIP = 10.0.0.20 then fwd(3) + 
      if dstIP = 10.0.0.30 then fwd(1) + 
      if dstIP = 10.0.0.40 then fwd(1) + 
      if dstIP = 10.0.0.50 then fwd(1)
    end

let routing_for_104 =
  if switch = 104 then 
    begin 
      if dstIP = 10.0.0.10 then fwd(1) + 
      if dstIP = 10.0.0.20 then fwd(1) + 
      if dstIP = 10.0.0.30 then fwd(2) + 
      if dstIP = 10.0.0.40 then fwd(3) + 
      if dstIP = 10.0.0.50 then fwd(1)
    end


let sol = 
  if frameType = arp then 
    all
  else 
    begin 
      routing_for_101 + 
      routing_for_102 + 
      routing_for_103 + 
      routing_for_104
    end