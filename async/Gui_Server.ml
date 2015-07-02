open Core.Std
open Async.Std

type handler = body:Cohttp_async.Body.t -> Unix.Socket.Address.Inet.t -> 
  Cohttp_async.Request.t -> Cohttp_async.Server.response Deferred.t

type routes = (bytes * (bytes array -> handler Deferred.t)) list

module Topo = Frenetic_NetKAT_Net.Net.Topology
module VertexMap = Map.Make(String)

let static_handler ?content_type (filename: bytes) = fun ~body _ _ ->
  let headers = match content_type with
    | None -> None
    | Some(typ) ->
      Some(Cohttp.Header.init_with "Content-type" typ) in
  Cohttp_async.Server.respond_with_file ?headers filename

let bytes_handler (b: bytes) : handler = fun ~body _ _ ->
  Cohttp_async.Server.respond_with_string "hi"

let string_handler str : handler = fun ~body _ _ ->
  Cohttp_async.Server.respond_with_string str

let not_found_handler : handler = fun ~body _ _ ->
  Cohttp_async.Server.respond_with_string ~code:(`Code 404) "Not found"

let routes = [
  ("/",
    fun _ -> return( static_handler "static/index.html"));
  (* XXX(seliopou): These are very, very bad patterns for a route, as they
   * espose the entire filesystem. It'll do for a demo. *)
  ("/static/(.*\\.svg)",
    fun g ->
        return (static_handler ~content_type:"image/svg+xml" ("static/" ^ (Array.get g 1))));
  ("/static/(.*)",
    fun g ->
        return (static_handler ("static/" ^ (Array.get g 1))))
]

let routes_to_handler rs: handler =
  let table = List.map rs ~f:(fun (route, handler) ->
    printf "Compiled \"%s\"" route;
    (Re_posix.(compile (re ("^" ^ route ^ "$"))), handler)) in
  let rec loop uri t =
    begin match t with
      | (re, handler)::t' ->
        begin try (handler Re.(get_all (exec re uri)))
          with Not_found -> loop uri t'
        end
      | [] ->
        return (fun ~body _ _ ->
          Cohttp_async.Server.respond_with_string ~code:(`Code 404)
            "Not found") end in
  fun ~body addr (request: Cohttp_async.Request.t) ->
    (loop (Uri.path (Cohttp_async.Request.uri request)) table) >>= fun handler ->
handler body addr request

let create ?max_connections ?max_pending_connections
    ?buffer_age_limit ?on_handler_error ext_routes =
  (* routes_to_handler (routes @ ext_routes)  >>= fun h -> *)
  Cohttp_async.Server.create ?max_connections ?max_pending_connections
    ?buffer_age_limit ?on_handler_error
    (Tcp.on_port 8080) (routes_to_handler (routes @ ext_routes))

