(* KATNetic is a more civilized name than what Nate suggested. *)

module Example = struct
  open SDN_Types
  open VInt
  open NetKAT_Types

  let fwd_by_port sw src dst =
    Seq (Filter (And (Test (Switch, Int64 sw), Test (Header InPort, Int16 src))),
    	   Mod (Header InPort, Int16 dst))

  let pol = Par (fwd_by_port 1L 1 2, fwd_by_port 1L 2 1)

end

let () =
  Lwt_main.run (Controller.start 6633 (NetCore_Stream.constant Example.pol))
