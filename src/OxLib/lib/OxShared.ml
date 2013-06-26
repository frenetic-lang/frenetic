open Printf
open Packet
open OpenFlow0x01_Core
open OpenFlow0x01

module Log = Lwt_log

module Platform = OpenFlow0x01_Platform

type to_sw = switchId * xid * Message.t

let (to_send_stream, defer) : (to_sw Lwt_stream.t * (to_sw option -> unit))
    = Lwt_stream.create ()

let munge_exns thunk =
  try_lwt
    Lwt.wrap thunk
  with exn ->
    begin
      Log.error_f ~exn:exn "unhandled exception raised by a callback" >>
      Lwt.return ()
    end
