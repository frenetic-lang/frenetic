(* A larger network of switches.
   Flood arp packets.
   Route IP packets along topology.

   Mininet Topology: linear,3

    h1        h2        h3
    |         |         |
    |         |         |
    1         1         1
   (s1)2 -- 2(s2)3 -- 2(s3)

   To Execute:

   $ sudo mn --controller=remote --topo=linear,3
   mininet> h1 ping -c 1 h3
*)

let s1 =
  if switch = 1 then 
    if dstIP = 10.0.0.1 then fwd(1)
    else fwd(2)

let s2 =
  if switch = 2 then 
    if dstIP = 10.0.0.1 then fwd(2)
    else if dstIP = 10.0.0.2 then fwd(1)
    else fwd(3)
   
let s3 =
  if (switch = 3) then 
    if dstIP = 10.0.0.1 || dstIP = 10.0.0.2 then fwd(2)
    else fwd(1)

let router =
  if frameType = arp then all
  else s1 + s3   (* missing s2 *)

let monitored_network = 
    router 
  + if switch = 2 && frameType = ip then monitorPackets("S2")
  + if switch = 3 && frameType = ip then monitorPackets("S3")

(*
let monitored_network = 
  router + if switch = 3 then monitorLoad(5, "LOAD" )
*)

monitored_network
