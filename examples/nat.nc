let translate_private, translate_public = nat (publicIP = 192.168.2.1) in
   (if switch = 1 && inPort = 1 then 
      (translate_private; if inPort = 1 then 2 else pass)
    else
      drop) |
   (if switch = 1 && inPort = 2 then
      (translate_public; if inPort = 2 then 1 else pass)
    else
      drop)
