(* a simple, static port mapping.
   test in mininet on the default topology as follows: 

   mininet> h2 iperf -s -p 5010 &
   mininet> h1 iperf -c 10.0.0.2 -p 5020

   You should see h1 establish a connection with h2.
*)

let mapper =
  if inPort = 1 && tcpDstPort = 5020 then
    tcpDstPort 5020 -> 5010
  else if inPort = 2 && tcpSrcPort = 5010 then
    tcpSrcPort 5010 -> 5020
  else
    pass

let forwarder =
  mapper; all
  