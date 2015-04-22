open Core.Std
open Async.Std

module Std = struct

  type policy = NetKAT_Types.policy
  type pred = NetKAT_Types.pred
  type packet = Frenetic_Packet.packet
  type dlAddr = Frenetic_Packet.dlAddr

  let run_static (pol : policy) : never_returns =
    let main () =
      let app = Async_NetKAT.Policy.create_static pol in
      Async_NetKAT_Controller.start app () >>> fun _ -> ()
    in
    Scheduler.go_main ~main ()

end
