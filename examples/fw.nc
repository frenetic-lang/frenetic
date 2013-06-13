(* stateful firewall:  to run on default 1-switch network *)

if dlTyp = arp then all
else monitorTable(1,fw(1,1,2))