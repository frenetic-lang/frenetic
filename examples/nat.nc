let translate_private, translate_public = nat (publicIP = 192.168.2.1) in
   (if switch = 1 && inPort = 1 then translate_private; 2) |
   (if switch = 1 && inPort = 2 then translate_public; 1)
