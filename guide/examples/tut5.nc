(* A Network Address Translator

 To execute:

 $ sudo mn --controller=remote --mac
 mininet> h2 arp -s 10.0.0.254 00:00:00:00:00:01
 mininet> h2 python -m SimpleHTTPServer 80 . &
 mininet> h1 wget -O - 10.0.0.2

*)

let natter =
  let translatePrivate, translatePublic = 
    nat (publicIP = 10.0.0.254) 
  in
    if switch = 1 && inPort = 1 then 
      (translatePrivate; if inPort = 1 then fwd(2) else pass)
  + if switch = 1 && inPort = 2 then
      (translatePublic; if inPort = 2 then fwd(1) else pass)

let app =
  if frameType = arp then all
  else monitorTable(1, natter)       