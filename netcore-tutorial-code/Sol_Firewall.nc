(* Solution to NetCore Chapter 1, Exercise 1. *)

if inPort = 1 && dlTyp = arp then all 
else if inPort = 2 then all
else if (inPort = 3 || inPort = 4 || inPort = 5)
        && (dlTyp = arp 
           || (dlTyp = ip 
              && nwProto = tcp
              && tcpDstPort = 80))
     then all
else if inPort = 5 
        && (nwProto = icmp
           || (nwProto = tcp && tcpDstPort = 22))
     then all
else drop
