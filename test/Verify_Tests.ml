open Types
open VInt
open NetCore_Verify
open NetCore_Verify_Util
open SDN_Types
open Sat



TEST "setup-works" = 
    Sat.solve (Sat.ZProgram ([])) 

TEST "declare-var-works" = 
    Sat.solve (let _ = Sat.fresh Sat.SPacket in Sat.ZProgram ([])) 


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
  verify_k 1 "narrowing down2"
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

  TEST "range" = 
  List.length (Verify.range 0 5) = 6

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

	TEST "retrict-waypoint-sanity" = 
  (verify_history "restrict_waypoint-sanity"
	 (make_packet_2 1 1)
	 pol_topo
	 noop_expr
	 (make_packet_2 3 1)
	 true)
 

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
*)
