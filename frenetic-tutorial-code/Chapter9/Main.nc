(* Modify this file to include your monitoring infrastructure *)

include "../Chapter8/Sol_Firewall.nc"

(* A forwarding solution to Chapter 8 -- use your own if you wish *)
let forwarding =
  if switch = 1 then
    if inPort = 1 then fwd(2)
    else if inPort = 2 then fwd(1)
    else drop
  else if switch = 2 then
    if dlDst = 00:00:00:00:00:01 then fwd(1)
    else if dlDst = 00:00:00:00:00:02 then fwd(2)
    else fwd(3)
  else if switch = 3 then
    if dlDst = 00:00:00:00:00:03 then fwd(1)
    else if dlDst = 00:00:00:00:00:04 then fwd(2)
    else fwd(3)
  else
    drop

(* Implement a monitor for the web traffic h2 sends to each other host *)
let monitoring = drop

(* define app by composing the definitions: firewall, forwarding, monitoring *)  
let app = drop

monitorTable(2, app)