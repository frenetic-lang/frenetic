open Core.Std
open Async.Std

type handler = body:Cohttp_async.Body.t -> Async_extra.Import.Socket.Address.Inet.t ->
  Cohttp_async.Request.t -> Cohttp_async.Server.response Deferred.t

type routes = (string * (string array -> handler Deferred.t)) list

val static_handler : ?content_type:string -> string -> handler

val bytes_handler : string -> handler

val string_handler : string -> handler

val not_found_handler : handler
(*val delta_events_to_json : gui_event list -> Frenetic_NetKAT_Net.Net.Topology.t -> string *)

val create :
  ?max_connections: int ->
  ?max_pending_connections: int ->
  ?buffer_age_limit: Writer.buffer_age_limit ->
  ?on_handler_error: [ `Call of Async_extra.Import.Socket.Address.Inet.t -> exn
  -> unit | `Ignore | `Raise ] -> routes ->
  (Async_extra.Import.Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t   (* route, fun urlparams -> handler *)
