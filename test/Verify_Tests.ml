open NetKAT_Types
open VInt
open NetCore_Verify
open NetCore_Verify_Util
TEST "simple-check" = 
  verify "are tests even running" 
	(make_packet 1 1)
	(make_simple_topology (make_transition 1 1 2 1))
	(make_packet 2 1)
	true
	

