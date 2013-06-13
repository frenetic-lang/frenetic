let firewall_compact =
      (* The first two lines make the flow table much larger! You can remove,
         but that allows mac addresses other than {1,2,3,4} access to
         SSH,HTTP, SMTP *) 
  if (dlSrc=::1 || dlSrc=::2 || dlSrc=::3 || dlSrc=::4) &&
     (dlDst=::1 || dlDst=::2 || dlDst=::3 || dlDst=::4) &&
     (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25) &&
     !(dlDst = ::3 && (dlSrc = ::1 || dlSrc = ::2)) && (* block to 3 *)
     (* encoded implication *)
     (!((dlSrc = ::1 || dlSrc = ::2) && dlDst = ::4) || tcpDstPort = 80) &&
     (tcpDstPort = 25 || !((dlSrc = ::4 || dlSrc=::3) && dlDst=::2))
    then pass
    else drop


let monitor =
  if tcpDstPort = 25 then
    fwd(666)
  else
    drop

let topo = 
  if dlDst = ::1 then fwd(1)
  else if dlDst = ::2 then fwd(2)
  else if dlDst = ::3 then fwd(3)
  else if dlDst = ::4 then fwd(4)
  else drop

let nat_private =
  if tcpSrcPort = 8080 then
    tcpSrcPort 8080 -> 80
  else
    pass

let nat_public =
  if tcpDstPort = 80 then
    tcpDstPort 80 -> 8080
  else
    pass

let nat =
  if inPort = 1 then nat_private
  else nat_public

let foo = 
  monitorTable(0, monitor + (firewall_compact; nat; topo))

foo
