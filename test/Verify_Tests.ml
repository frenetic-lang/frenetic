open NetKAT_Types
module SDN = SDN_Types
open VInt
open NetCore_Verify

let verify (description: string) (initial_state: policy) (program: policy) (final_state: policy) (desired_outcome: bool): bool = 
	check description initial_state program final_state (Some desired_outcome)

TEST "simple-check" = 
  verify "are tests even running" Id (Mod (Switch, (VInt.Int64 (Int64.of_int 12)))) (Test (Switch, (VInt.Int64 (Int64.of_int 12)))) true
  

