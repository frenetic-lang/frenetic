(* A larger network of switches.
   Flood arp packets.
   Route IP packets along topology.

   Mininet Topology: linear,3

    h1        h2        h3
    |         |         |
    |         |         |
    1         1         1
   (s1)2 -- 2(s2)3 -- 2(s3)

*)

let s1 =
  filter (switch = 1);
  if dstIP = 10.0.0.1 then fwd(1)
  else fwd(2)

let s2 =
  filter (switch = 2);
  if dstIP = 10.0.0.1 then fwd(2)
  else if dstIP = 10.0.0.2 then fwd(1)
  else fwd(3)
    
let s3 =
  filter (switch = 3);
  if dstIP = 10.0.0.1 || dstIP = 10.0.0.2 then fwd(2)
  else fwd(1)

let router =
  if frameType = arp then all
  else s1 + s3   (* bug! should be s1 + s2 + s3 *)


let monitored_network = 
  router + monitorPackets(switch=2)

(*
let monitored_network = 
  router + monitorPackets(switch=3)
*)

(*
let monitored_network = 
  router + monitorLoad(5, switch = 3)
*)
