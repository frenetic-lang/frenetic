(* Solution to NetCore Chapter 1, Exercise 1. *)

if inPort = 1 && frameType = arp then all 
else if inPort = 2 then all
else if (inPort = 3 || inPort = 4 || inPort = 5)
        && (frameType = arp 
           || (frameType = ip 
              && ipProtocol = tcp
              && tcpDstPort = 80))
     then all
else if inPort = 5 
        && (ipProtocol = icmp
           || (ipProtocol = tcp && tcpDstPort = 22))
     then all
else drop
