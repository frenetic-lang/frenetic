(*

 This is not the ideal way to run it. I'll think some more...

 $ sudo mn --controller=remote --mac
 mininet> h2 arp -s 10.0.0.254 00:00:00:00:00:01
 mininet> h2 python -m SimpleHTTPServer 80 . &
 mininet> h1 curl 10.0.0.2

*)



if frameType = arp then
  if inPort = 2 then fwd(1)
  else (if inPort = 1 then fwd(2))
else  
  begin 
    let translatePrivate, translatePublic = 
      nat (publicIP = 10.0.0.254) in
    let pol = 
        if switch = 1 && inPort = 1 then 
          (translatePrivate; if inPort = 1 then fwd(2) else pass)
      + if switch = 1 && inPort = 2 then
          (translatePublic; if inPort = 2 then fwd(1) else pass) in 
    monitorTable(1, pol) 
  end  