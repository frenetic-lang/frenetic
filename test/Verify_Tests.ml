open NetKAT_Types
open VInt
open NetCore_Verify
open NetCore_Verify_Util


TEST "simple-check" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 1)))
	(make_packet_2 2 1)
	true

TEST "we care about p in (p;t)*" = 
	verify "we care about p in (p;t)*"
	(make_packet_2 1 1)
	(starify False (make_transition (1, 1) (2,1)))
	(make_packet_2 2 1)
	false

TEST "we love switch 2" = 
	verify "we love switch 2"
	  (make_packet_2 1 1)
	  (starify (Test (Switch, make_vint 2)) (make_transition (1, 1) (2,1)))
	  (make_packet_2 2 1)
	  false
	
TEST "neg has no effect" = 
	let strt = (make_packet_1 1) in
	let fnsh = (make_packet_1 2) in
	let topo = make_transition (1,1) (2,1) in
	(verify "we hate switch 1"
		 strt
		 (starify 
			(Neg (Test (Switch, make_vint 1)))
			topo)
		 fnsh
		 false)

TEST "simple-check-false" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 1)))
	(make_packet_2 2 2)
	false 
	
(*
TEST "restrict" = 
	let pol = (Seq (Test (Header SDN_Types.EthSrc, make_vint 1), Neg (Test (Switch, make_vint 2)))) in
	let tran1 = make_transition (1, 2) (2, 1) in
	let tran2 = make_transition (2, 3) (3, 2) in
	let tran3 = make_transition (3, 1) (1, 3) in
	let tran4 = make_transition (1, 3) (3, 1) in
	let topo = combine_topologies [tran1; tran2; tran3; tran4] in
	let pol_topo = starify pol topo in
	(verify "restrict1"
	   (make_packet_4 1 2 1 2)
	   pol_topo
	   (make_packet_2 2 1)
	   false)  &&
	  (verify "restrict2"
		 (make_packet_4 1 3 1 2)
		 pol_topo
		 (make_packet_2 3 1)
		 true)
	  
*)
