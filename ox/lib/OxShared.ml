open Async.Std

open Packet
open OpenFlow0x01_Core
open OpenFlow0x01

module Log = Async_OpenFlow.Log

let _ = Log.set_level `Info

let tags = [("openflow", "controller")]


type to_sw = switchId * xid * Message.t

let (pkt_out : to_sw Pipe.Reader.t), (defer : to_sw option -> unit) =
  let r, w = Pipe.create () in
  r, function
      | None -> ()
      | Some to_sw -> Pipe.write_without_pushback w to_sw

let munge_exns ?(name="munge_exns") thunk =
  let open Core.Std in
  Monitor.try_with ~name (fun () -> return (thunk ()))
  >>> function
    | Ok () -> ()
    | Error exn ->
      Log.error ~tags "unhandled exception raised by a callback\n%s"
        (Exn.to_string exn)
