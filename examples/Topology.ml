open Core.Std
open Async.Std

module Log = Async_OpenFlow.Log

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "reactive");
              ("openflow", "topology")]]

let main () =
  let open Async_OpenFlow.Platform.Trans in
  let open Async_OpenFlow0x01.Controller in
  create 6633 ()
  >>> fun t ->
    Deferred.don't_wait_for (Pipe.iter (run (features >=> switch_topology) t (listen t)) ~f:(fun _ ->
      return ()))

let _ = main ()
let _ = never_returns (Scheduler.go ())
