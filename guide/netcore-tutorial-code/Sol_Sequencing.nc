(*

           destination MAC address --------->
source   +----+---------------+---------------+---------------+---------------+
MAC      |    | 01            | 02            | 03            | 04            |
address  |----+---------------+---------------+---------------+---------------+
  |      | 01 | SSH,HTTP,SMTP | SSH,HTTP,SMTP | deny all      | HTTP          |
  |      | 02 | SSH,HTTP,SMTP | SSH,HTTP,SMTP | deny all      | HTTP          |
  V      | 03 | SSH,HTTP,SMTP | SMTP          | SSH,HTTP,SMTP | SSH,HTTP,SMTP |
         | 04 | SSH,HTTP,SMTP | SMTP          | SSH,HTTP,SMTP | SSH,HTTP,SMTP |
         +----+---------------+---------------+---------------+---------------+
*)

(* Let's give students a big table, such as the following .. *)
let firewall =
  if (srcMAC = ::1 && dstMAC = ::1 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = ::1 && dstMAC = ::2 && 
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
     (srcMAC = ::1 && dstMAC = ::4 && tcpDstPort = 80) ||
     (srcMAC = ::2 && dstMAC = ::1 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::2 && dstMAC = ::2 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::2 && dstMAC = ::4 && tcpDstPort = 80) ||
    (srcMAC = ::3 && dstMAC = ::1 && tcpDstPort = 2) ||
    (srcMAC = ::3 && dstMAC = ::1 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::3 && dstMAC = ::2 && tcpDstPort = 25) ||
    (srcMAC = ::3 && dstMAC = ::3 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::3 && dstMAC = ::4 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::4 && dstMAC = ::1 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::4 && dstMAC = ::2 && tcpDstPort = 25) ||
    (srcMAC = ::4 && dstMAC = ::3 && 
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25)) ||
    (srcMAC = ::4 && dstMAC = ::4 &&
      (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25))
  then
    all
  else
    drop

(* Then, in the exercise, ask them to write a smaller, more readable predicate.
   For example, below, we are allowing by default, then denying. *)
let firewall_compact =
      (* The first two lines make the flow table much larger! You can remove,
         but that allows mac addresses other than {1,2,3,4} access to
         SSH,HTTP, SMTP *) 
  if (srcMAC=::1 || srcMAC=::2 || srcMAC=::3 || srcMAC=::4) &&
     (dstMAC=::1 || dstMAC=::2 || dstMAC=::3 || dstMAC=::4) &&
     (tcpDstPort = 22 || tcpDstPort = 80 || tcpDstPort = 25) &&
     !(dstMAC = ::3 && (srcMAC = ::1 || srcMAC = ::2)) && (* block to 3 *)
     (* encoded implication *)
     (!((srcMAC = ::1 || srcMAC = ::2) && dstMAC = ::4) || tcpDstPort = 80) &&
     (tcpDstPort = 25 || !((srcMAC = ::4 || srcMAC=::3) && dstMAC=::2))
    then all
    else drop

let prog = monitorTable(0, firewall_compact)