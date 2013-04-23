open OpenFlow0x04Types
open WordInterface
open Platform0x04
open FaultTolerance
open NetCoreFT

module G = Graph.Graph

module Routing = struct

    (* Diamond topology *)
    (* S1
       /\
     S2 S3
      \ /
       S4
    *)

  let (policy, push) = Lwt_stream.create ()

  let ints_to_ipv4 (a,b,c,d) =
    let (|||) x y = Int32.logor x y in
    let (<<<) x y = Int32.shift_left x y in
    let a = Int32.of_int a in
    let b = Int32.of_int b in
    let c = Int32.of_int c in
    let d = Int32.of_int d in
    (a <<< 24) ||| (b <<< 16) ||| (c <<< 8) ||| d

  let make_host_ip i = ints_to_ipv4 (10,0,0,i)
  let h1 = make_host_ip 1
  let h2 = make_host_ip 2
  let h3 = make_host_ip 3
  let h4 = make_host_ip 4

  let s1 = Int64.of_int 1
  let s2 = Int64.of_int 2
  let s3 = Int64.of_int 3
  let s4 = Int64.of_int 4

  let toPort i = To (Int32.of_int i)

  let from_to i j = And (SrcIP i, DstIP j)
			   (* (DlType 0x800)) *)
  let make_policy =
    Par (Par (Par (Par (Par (Pol (And (from_to h1 h2, 
				       Switch s1),
				  [toPort 2]),
			Pol (And (from_to h1 h2, 
				  Switch s2),
			     [toPort 2])),
		   Pol (And (from_to h1 h2, 
			     Switch s4),
			[toPort 1])),
	      Pol (And (from_to h2 h1, 
			Switch s1),
		   [toPort 1])),
	 Pol (And (from_to h2 h1, 
		   Switch s2),
	      [toPort 1])),
    Pol (And (from_to h2 h1, 
	      Switch s4),
	 [toPort 2]))


  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let () = let pol = make_policy in
	   Printf.printf "%s\n" (policy_to_string pol);
	   push (Some (pol, NetCoreFT.Pol (All, []), NetCoreFT.Pol (All, [])))
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCoreFT.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

