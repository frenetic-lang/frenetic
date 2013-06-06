(* a simple, static port mapping.
   test in mininet on the default topology as follows: 

   mininet> h2 iperf -s -p 22 &
   mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 0.0001

   You should see h1 establish a connection with h2.
*)

let mapper =
  if inPort = 1 && tcpDstPort = 5022 then
    tcpDstPort 5022 -> 22
  else if inPort = 2 && tcpSrcPort = 22 then
    tcpSrcPort 22 -> 5022
  else
    pass

let forwarder =
  mapper; all
