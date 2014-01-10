open Types
open VInt
open NetKAT_Verify_Reachability
open NetKAT_Verify_Tools
open SDN_Types
open NetKAT_Sat.Sat_Utils




TEST "simple-check" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 2)))
	(make_packet_2 2 2)
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
		 false )

TEST "simple-check-false" = 
  verify "are tests even running" 
	(make_packet_2 1 1)
	(make_simple_topology (make_transition (1, 1) (2, 1)))
	(make_packet_2 2 2)
	false 

TEST "narrowing down1" = 
  verify "narrowing down1"
    (make_packet_2 1 1)
    (starify 
       (Neg (Test (Switch, make_vint 1))) 
       (combine_topologies 
	  [(make_transition (1, 1) (2, 1)); 
	   (make_transition (2, 1) (3, 1))]
       ))
    (make_packet_2 3 1)
    false


TEST "narrowing down2" = 
  verify  "narrowing down2"
    (make_packet_2 1 1)
    (starify 
       (Test (Switch, make_vint 1)) 
       (combine_topologies 
	  [(make_transition (1, 1) (2, 2)); 
	   (make_transition (2, 2) (3, 3))]
       ))
    (make_packet_2 3 3)
    false

TEST "narrowing down3" = 
  verify "narrowing down2"
    (make_packet_2 1 1)
    (starify 
       (Test (Switch, make_vint 1)) 
       (combine_topologies 
	  [(make_transition (1, 1) (2, 2)); 
	   (make_transition (2, 2) (3, 3))]
       ))
    (make_packet_2 3 3)
    false

let ethsrc_is_1 = Test (Header SDN_Types.EthSrc, make_vint 1)
let switch_is_2 = Test (Switch, make_vint 2)
let pol = Or (And (ethsrc_is_1, Neg (switch_is_2)), Neg (ethsrc_is_1) ) 
let tran1 = make_transition (1, 1) (2, 1)
let tran2 = make_transition (2, 1) (3, 1)
let topo = combine_topologies [tran1; tran2]
let pol_topo = starify pol topo


  TEST "dijkstra" = 
  (dijkstra_test topo) = 2

  TEST "restrict0" = 
  (verify "restrict0"
	 (make_packet_2 1 1)
	 (make_simple_topology topo)
	 (make_packet_2 2 1)
	 true )

  TEST "restrict1" = 
  (verify "restrict1"
	 (make_packet_4 1 1 1 3)
	 (make_simple_topology topo)
	 (make_packet_2 3 1)
	 true )
  

	TEST "restrict2" = 
  (verify "restrict2"
	 (make_packet_4 2 1 1 3)
	 pol_topo
	 (make_packet_2 3 1)
	 false )

	TEST "restrict3" = 
  (verify "restrict3"
	 (make_packet [])
	 pol_topo
	 (make_packet_2 3 1)
	 true )


	TEST "restrict4" = 
  (verify "restrict4"
	 (make_packet_1 2)
	 (make_simple_topology topo)
	 (make_packet_2 3 1)
	 true )

	TEST "restrict5" = 
  (verify "restrict5"
	 (make_packet_4 1 1 1 2)
	 pol_topo
	 (make_packet_2 2 1)
	 true )

	TEST "restrict6" = 
  (verify "restrict6"
	 (make_packet_2 1 1)
	 pol_topo
	 (make_packet_2 3 1)
	 true )

    TEST "restrict7" = 
  (verify "restrict7"
     (make_packet_2 1 1)
     pol_topo
     (make_packet_2 4 1)
     false)

    TEST "real-restrict" = 
  (verify "real-restrict"
     (make_packet_3 1 1 1)
     pol_topo
     (make_packet_2 3 1)
     false )

    TEST "real-restrict2" = 
  (verify "real-restrict"
     (make_packet_3 1 1 2)
     pol_topo
     (make_packet_2 3 1)
     true )

    TEST "why isn't this over-constraining?" = 
  (verify "why isn't this over-constraining?"
     (make_packet_2 1 1)
     (make_simple_topology
	(combine_topologies 
	   [(make_transition (1, 1) (2, 2));
	    (make_transition (1, 1) (3, 3))]))
     (make_packet_2 2 2)
     true
  )

(*
	TEST "restrict6-history" = 
  (verify_history "restrict6-history"
	 (make_packet_2 1 1)
	 pol_topo
	 noop_expr
	 (make_packet_2 3 1)
	 true )

	TEST "easy-history-check" = 
  (verify_history "easy-history-check"
	 (make_packet_2 1 1)
	 pol_topo
	 (equal_fields "EthSrc")
	 (make_packet_2 3 1)
	 true)

	TEST "easy-history-check-2" = 
  (verify_history "easy-history-check-2"
	 (make_packet_2 1 1)
	 pol_topo
	 (fold_pred_and (Test (Header SDN_Types.EthSrc, make_vint 1)))
	 (make_packet_2 3 1)
	 false)

	TEST "easy-history-check-3" = 
  (verify_history "easy-history-check-3"
	 (make_packet_2 1 1)
	 pol_topo
	 (fold_pred_and (Test (Header SDN_Types.EthSrc, make_vint 2)))
	 (make_packet_2 3 1)
	 true)

  )()

	
TEST "exactly-two-hops" = 
  (verify_history "exactly-two-hops"
	 (make_packet_2 1 1)
	 pol_topo
	 (fun l -> if List.length l = 3 then bool_to_z3 true else bool_to_z3 false )
	 (make_packet_2 3 1)
	 true)


	TEST "not-three-hops" = 
  (verify_history "not-three-hops"
	 (make_packet_2 1 1)
	 pol_topo
	 (fold_pred_or_with_counter 
		(fun n -> 
		  if n = 3 then True else False))
	 (make_packet_2 3 1)
	 false)

	TEST "retrict-waypoint-sanity" = 
  (verify_waypoint "restrict_waypoint-sanity"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (make_transition (1,1) (2,2)))
	 (make_packet_2 2 2)
	 (make_packet_2 3 3)
	 false)

	TEST "retrict-waypoint-sanity2" = 
  (verify_waypoint "restrict_waypoint-sanity2"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (make_transition (1,1) (2,2)))
	 (make_packet_2 2 2)
	 (make_packet_2 1 1)
	 true
  ) 


	TEST "retrict-waypoint-sanity2" = 
  (verify_waypoint "restrict_waypoint-sanity2"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (combine_topologies
	    [(make_transition (1,1) (2,2));
	     (make_transition (2,2) (3,3))]))
	 (make_packet_2 3 3)
	 (make_packet_2 2 2)
	 true
  )

	TEST "retrict-waypoint1" = 
  (verify_waypoint "restrict_waypoint1"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (combine_topologies
	    [(make_transition (1,1) (2,2));
	     (make_transition (2,2) (3,3));
	     (make_transition (1,1) (3,3))]))
	 (make_packet_2 3 3)
	 (make_packet_2 2 2)
	 false
  )

    TEST "reachability-waypoint1" = 
  (verify "restrict_waypoint2"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (combine_topologies
	    [(make_transition (1,1) (2,2));
	     (make_transition (2,2) (4,4));
	     (make_transition (1,1) (3,3));
	     (make_transition (3,3) (4,4))]))
	 (make_packet_2 4 4)
	 true)

	TEST "retrict-waypoint2" = 
  (verify_waypoint "restrict_waypoint2"
	 (make_packet_2 1 1)
	 (make_simple_topology
	    (combine_topologies
	    [(make_transition (1,1) (2,2));
	     (make_transition (2,2) (4,4));
	     (make_transition (1,1) (3,3));
	     (make_transition (3,3) (4,4))]))
	 (make_packet_2 4 4)
	 (make_packet_2 3 3)
	 false
  )

	TEST "retrict-waypoint3" = 
  (verify_waypoint "restrict_waypoint3"
	 (make_packet_2 1 1)
	 (starify
	    (Neg switch_is_2)
	    (combine_topologies
	    [(make_transition (1,1) (2,2));
	     (make_transition (2,2) (4,4));
	     (make_transition (1,1) (3,3));
	     (make_transition (3,3) (4,4))]))
	 (make_packet_2 4 4)
	 (make_packet_2 3 3)
	 true
  )
*)

(*  let check_slice_isolation str inpts pol outpts not_in_slice oko = 
    TEST "slice-isolation" = 
  (check_slice_isolation "slice-isolation" [make_switch 1; make_switch 2] pol_topo 
     [make_switch 1; make_switch 2; make_switch 3] [make_switch 15] true)

*)
 
(*
	TEST "restrict_waypoint1" = 
  (verify_history "restrict_waypoint1.1"
	 (make_packet_2 1 1)
	 pol_topo
	 (no_waypoint_expr 2)
	 (make_packet_2 3 1)
	 false) &&
	(verify_history "restrict_waypoint1.2"
	   (make_packet_2 1 1)
	   pol_topo
	   (exists_waypoint_in_one_history 2)
	   (make_packet_2 3 1)
	    true)

	TEST "not_waypoint" = 
  (fun () -> (* anonymous namespace, the only way I know how *)
   let tran1 = make_transition (1, 1) (2 , 1) in
   let tran2 = make_transition (2, 1) (3, 1) in
   let tran3 = make_transition (1, 1) (4, 1) in
   let tran4 = make_transition (4, 1) (3, 1) in
   let topo = make_simple_topology ( combine_topologies [tran1; tran2; tran3; tran4]) in
   (verify_history "not_waypoint_1"
	  (make_packet_2 1 1)
	  topo
	  (no_waypoint_expr 2)
	  (make_packet_2 3 1)
	  true)
*)




let topology_0 = (combine_topologies
[
(make_transition (0,0) (1,1))
])
let topology_10 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
])
let topology_20 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
])
let topology_30 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
])
let topology_40 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
])
let topology_50 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
;(make_transition (40,40) (41,41))
;(make_transition (41,41) (42,42))
;(make_transition (42,42) (43,43))
;(make_transition (43,43) (44,44))
;(make_transition (44,44) (45,45))
;(make_transition (45,45) (46,46))
;(make_transition (46,46) (47,47))
;(make_transition (47,47) (48,48))
;(make_transition (48,48) (49,49))
;(make_transition (49,49) (50,50))
])
let topology_60 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
;(make_transition (40,40) (41,41))
;(make_transition (41,41) (42,42))
;(make_transition (42,42) (43,43))
;(make_transition (43,43) (44,44))
;(make_transition (44,44) (45,45))
;(make_transition (45,45) (46,46))
;(make_transition (46,46) (47,47))
;(make_transition (47,47) (48,48))
;(make_transition (48,48) (49,49))
;(make_transition (49,49) (50,50))
;(make_transition (50,50) (51,51))
;(make_transition (51,51) (52,52))
;(make_transition (52,52) (53,53))
;(make_transition (53,53) (54,54))
;(make_transition (54,54) (55,55))
;(make_transition (55,55) (56,56))
;(make_transition (56,56) (57,57))
;(make_transition (57,57) (58,58))
;(make_transition (58,58) (59,59))
;(make_transition (59,59) (60,60))
])
let topology_70 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
;(make_transition (40,40) (41,41))
;(make_transition (41,41) (42,42))
;(make_transition (42,42) (43,43))
;(make_transition (43,43) (44,44))
;(make_transition (44,44) (45,45))
;(make_transition (45,45) (46,46))
;(make_transition (46,46) (47,47))
;(make_transition (47,47) (48,48))
;(make_transition (48,48) (49,49))
;(make_transition (49,49) (50,50))
;(make_transition (50,50) (51,51))
;(make_transition (51,51) (52,52))
;(make_transition (52,52) (53,53))
;(make_transition (53,53) (54,54))
;(make_transition (54,54) (55,55))
;(make_transition (55,55) (56,56))
;(make_transition (56,56) (57,57))
;(make_transition (57,57) (58,58))
;(make_transition (58,58) (59,59))
;(make_transition (59,59) (60,60))
;(make_transition (60,60) (61,61))
;(make_transition (61,61) (62,62))
;(make_transition (62,62) (63,63))
;(make_transition (63,63) (64,64))
;(make_transition (64,64) (65,65))
;(make_transition (65,65) (66,66))
;(make_transition (66,66) (67,67))
;(make_transition (67,67) (68,68))
;(make_transition (68,68) (69,69))
;(make_transition (69,69) (70,70))
])
let topology_80 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
;(make_transition (40,40) (41,41))
;(make_transition (41,41) (42,42))
;(make_transition (42,42) (43,43))
;(make_transition (43,43) (44,44))
;(make_transition (44,44) (45,45))
;(make_transition (45,45) (46,46))
;(make_transition (46,46) (47,47))
;(make_transition (47,47) (48,48))
;(make_transition (48,48) (49,49))
;(make_transition (49,49) (50,50))
;(make_transition (50,50) (51,51))
;(make_transition (51,51) (52,52))
;(make_transition (52,52) (53,53))
;(make_transition (53,53) (54,54))
;(make_transition (54,54) (55,55))
;(make_transition (55,55) (56,56))
;(make_transition (56,56) (57,57))
;(make_transition (57,57) (58,58))
;(make_transition (58,58) (59,59))
;(make_transition (59,59) (60,60))
;(make_transition (60,60) (61,61))
;(make_transition (61,61) (62,62))
;(make_transition (62,62) (63,63))
;(make_transition (63,63) (64,64))
;(make_transition (64,64) (65,65))
;(make_transition (65,65) (66,66))
;(make_transition (66,66) (67,67))
;(make_transition (67,67) (68,68))
;(make_transition (68,68) (69,69))
;(make_transition (69,69) (70,70))
;(make_transition (70,70) (71,71))
;(make_transition (71,71) (72,72))
;(make_transition (72,72) (73,73))
;(make_transition (73,73) (74,74))
;(make_transition (74,74) (75,75))
;(make_transition (75,75) (76,76))
;(make_transition (76,76) (77,77))
;(make_transition (77,77) (78,78))
;(make_transition (78,78) (79,79))
;(make_transition (79,79) (80,80))
])
let topology_90 = (combine_topologies
[
(make_transition (0,0) (1,1))
;(make_transition (1,1) (2,2))
;(make_transition (2,2) (3,3))
;(make_transition (3,3) (4,4))
;(make_transition (4,4) (5,5))
;(make_transition (5,5) (6,6))
;(make_transition (6,6) (7,7))
;(make_transition (7,7) (8,8))
;(make_transition (8,8) (9,9))
;(make_transition (9,9) (10,10))
;(make_transition (10,10) (11,11))
;(make_transition (11,11) (12,12))
;(make_transition (12,12) (13,13))
;(make_transition (13,13) (14,14))
;(make_transition (14,14) (15,15))
;(make_transition (15,15) (16,16))
;(make_transition (16,16) (17,17))
;(make_transition (17,17) (18,18))
;(make_transition (18,18) (19,19))
;(make_transition (19,19) (20,20))
;(make_transition (20,20) (21,21))
;(make_transition (21,21) (22,22))
;(make_transition (22,22) (23,23))
;(make_transition (23,23) (24,24))
;(make_transition (24,24) (25,25))
;(make_transition (25,25) (26,26))
;(make_transition (26,26) (27,27))
;(make_transition (27,27) (28,28))
;(make_transition (28,28) (29,29))
;(make_transition (29,29) (30,30))
;(make_transition (30,30) (31,31))
;(make_transition (31,31) (32,32))
;(make_transition (32,32) (33,33))
;(make_transition (33,33) (34,34))
;(make_transition (34,34) (35,35))
;(make_transition (35,35) (36,36))
;(make_transition (36,36) (37,37))
;(make_transition (37,37) (38,38))
;(make_transition (38,38) (39,39))
;(make_transition (39,39) (40,40))
;(make_transition (40,40) (41,41))
;(make_transition (41,41) (42,42))
;(make_transition (42,42) (43,43))
;(make_transition (43,43) (44,44))
;(make_transition (44,44) (45,45))
;(make_transition (45,45) (46,46))
;(make_transition (46,46) (47,47))
;(make_transition (47,47) (48,48))
;(make_transition (48,48) (49,49))
;(make_transition (49,49) (50,50))
;(make_transition (50,50) (51,51))
;(make_transition (51,51) (52,52))
;(make_transition (52,52) (53,53))
;(make_transition (53,53) (54,54))
;(make_transition (54,54) (55,55))
;(make_transition (55,55) (56,56))
;(make_transition (56,56) (57,57))
;(make_transition (57,57) (58,58))
;(make_transition (58,58) (59,59))
;(make_transition (59,59) (60,60))
;(make_transition (60,60) (61,61))
;(make_transition (61,61) (62,62))
;(make_transition (62,62) (63,63))
;(make_transition (63,63) (64,64))
;(make_transition (64,64) (65,65))
;(make_transition (65,65) (66,66))
;(make_transition (66,66) (67,67))
;(make_transition (67,67) (68,68))
;(make_transition (68,68) (69,69))
;(make_transition (69,69) (70,70))
;(make_transition (70,70) (71,71))
;(make_transition (71,71) (72,72))
;(make_transition (72,72) (73,73))
;(make_transition (73,73) (74,74))
;(make_transition (74,74) (75,75))
;(make_transition (75,75) (76,76))
;(make_transition (76,76) (77,77))
;(make_transition (77,77) (78,78))
;(make_transition (78,78) (79,79))
;(make_transition (79,79) (80,80))
;(make_transition (80,80) (81,81))
;(make_transition (81,81) (82,82))
;(make_transition (82,82) (83,83))
;(make_transition (83,83) (84,84))
;(make_transition (84,84) (85,85))
;(make_transition (85,85) (86,86))
;(make_transition (86,86) (87,87))
;(make_transition (87,87) (88,88))
;(make_transition (88,88) (89,89))
;(make_transition (89,89) (90,90))
])

TEST "benchmark" = 
  (fun () -> 
    let inpt = (make_packet_2 2 2) in
    let outp = (make_packet_2 1 1) in
    let start_time : float = Unix.time () in 
    let _ = (verify "1" inpt (make_simple_topology topology_0) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "10" inpt (make_simple_topology topology_10) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "20" inpt (make_simple_topology topology_20) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "30" inpt (make_simple_topology topology_30) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "40" inpt (make_simple_topology topology_40) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "50" inpt (make_simple_topology topology_50) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "60" inpt (make_simple_topology topology_60) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "70" inpt (make_simple_topology topology_70) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "80" inpt (make_simple_topology topology_80) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));
    let start_time : float = Unix.time () in 
    let _ = (verify "90" inpt (make_simple_topology topology_90) outp false) in
    print_string (Printf.sprintf "time: %f\n" (Unix.time () -. start_time));

    
  ) (); true
  


