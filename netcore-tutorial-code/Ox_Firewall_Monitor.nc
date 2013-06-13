(* A NetCore version of ox-tutorial-code/Sol_Firewall1.ml that blocks SSH
   traffic and monitors web traffic.

   $ sudo mn --controller=remote
   
   -- Should fail:

     mininet> h1 ping -c 1 h2

   -- Should succeed:

     mininet> h1 iperf -s -p 22 &
     mininet> h2 iperf -c 10.0.0.1 -p 22
*)

let firewall = if !(tcpDstPort = 22) then all in
let monitor = if srcIP = 10.0.0.1 then monitorLoad(10, "From H1") in
monitorTable(1, firewall + monitor)
