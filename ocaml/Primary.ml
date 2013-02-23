open NetCore

(* The semantics of primary/backup are a bit confusing. Because it's
   not very declarative, it's hard to understand the intent. I.e. how do
   I know when the primary policy has failed and I should move to the
   backup? I interpret the primary policy to have failed when it says to
   forward out a port that is down.  However, naive implementation of
   this can lead to VERY funky behavior: (Pol NoPackets [To 1]) == (Pol
   NoPackets []), but we may detect the first policy as having failed
   when port 1 goes down. Food for thought *)

type fault_tolerant_policy = 
  | PBPolicy of policy * policy

