(* A network of 3 switches.
   Flood arp packets.
   Route IP packets along topology.
   Prevent IP 10.0.0.2 from contacting IP 10.0.0.3 on tcp port 80 (web)

   Mininet Topology: linear,3

    h1        h2        h3
    |         |         |
    |         |         |
    1         1         1
   (s1)2 -- 2(s2)3 -- 2(s3)

   To Execute:

   $ sudo mn --controller=remote --topo=linear,3
   mininet> h1 ping -c 1 h3
   mininet> h2 ping -c 1 h3
   mininet> h3 ping -c 1 h1
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
    if dstIP = 10.0.0.3 then fwd(1)
    else fwd(2)

let firewall =
  if srcIP = 10.0.0.2 && dstIP = 10.0.0.3 && tcpDstPort = 80 then drop
  else pass

let router =
  if dlTyp = arp then all
  else (firewall; (s1 + s2 + s3))

router
