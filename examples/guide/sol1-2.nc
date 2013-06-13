(* Static NetCore Exercises Solution 2 *)

let router = 
    all
  + if frameType = arp then monitorLoad (5, "ARP")
  + if frameType = ip then monitorLoad (5, "IP")

router
