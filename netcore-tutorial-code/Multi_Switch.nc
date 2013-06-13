(* A larger network of switches.
   Flood arp packets.
   Route IP packets by destination IP.
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
*)

(* Destination-based IP routing for switches s1, s2 and s3 *)
let s1 = drop

let s2 = drop
   
let s3 = drop

(* Implement firewall preventing 10.0.0.1 from contacting 10.0.0.3 on port 80 *)
let firewall = drop

let router =
  if dlTyp = arp then all
  else drop     (* replace drop with policy using s1, s2, s3, firewall *)

router
