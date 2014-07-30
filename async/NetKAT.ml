open Core.Std
open Async.Std

module Std = struct

  type policy = NetKAT_Types.policy
  type pred = NetKAT_Types.pred

  let run_static (pol : policy) : never_returns =
    Async_NetKAT_Controller.start (Async_NetKAT.create_static pol) ();
    Scheduler.go ()

end