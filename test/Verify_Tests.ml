open NetKAT_Types
open VInt
open NetCore_Verify
open NetCore_Verify_Util

TEST "simple-check" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 1)))
	(make_packet_2 2 1)
	true 1

TEST "we care about p in (p;t)*" = 
	verify "we care about p in (p;t)*"
	(make_packet_2 1 1)
	(starify False (make_transition (1, 1) (2,1)))
	(make_packet_2 2 1)
	false 1

TEST "we love switch 2" = 
	verify "we love switch 2"
	  (make_packet_2 1 1)
	  (starify (Test (Switch, make_vint 2)) (make_transition (1, 1) (2,1)))
	  (make_packet_2 2 1)
	  false 1
	
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
		 false 1)

TEST "simple-check-false" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 1)))
	(make_packet_2 2 2)
	false 1



let ethsrc_is_1 = Test (Header SDN_Types.EthSrc, make_vint 1)
let switch_is_2 = Test (Switch, make_vint 2)
let pol = Or (And (ethsrc_is_1, Neg (switch_is_2)), Neg (ethsrc_is_1) ) 
let tran1 = make_transition (1, 1) (2, 1)
let tran2 = make_transition (2, 1) (3, 1)
let topo = combine_topologies [tran1; tran2]
let pol_topo = starify pol topo

  TEST "restrict0" = 
  (verify "restrict0"
	 (make_packet_4 1 1 1 2)
	 (make_simple_topology topo)
	 (make_packet_2 2 1)
	 true 3)
  TEST "restrict1" = 
  (verify "restrict1"
	 (make_packet_4 1 1 1 3)
	 (make_simple_topology topo)
	 (make_packet_2 3 1)
	 true 3)


	TEST "restrict2" = 
  (verify "restrict2"
	 (make_packet_4 1 1 1 3)
	 pol_topo
	 (make_packet_2 3 1)
	 false 3)

	TEST "restrict3" = 
  (verify "restrict3"
	 (make_packet [])
	 pol_topo
	 (make_packet_2 3 1)
	 true 5)


	TEST "restrict4" = 
  (verify "restrict4"
	 (make_packet_1 2)
	 (make_simple_topology topo)
	 (make_packet_2 3 1)
	 true 1)



