let routing =
  if dlDst=00:00:00:00:00:01 then
     fwd(1)
  (* [FILL] Replace "if false then drop" with route to hosts 2, 3, and 4. *)
  else if false then
   drop
  else if dlDst=ff:ff:ff:ff:ff:ff then
    all (*  allow broadcasts *)
  else
    drop
    
monitorTable(1, routing)

