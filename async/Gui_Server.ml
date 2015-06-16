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


let topo_to_json (t: Frenetic_NetKAT_Net.Net.Topology.t) =
  let module VertexMap = Map.Make(String) in 
  let vertex_to_json (v: Frenetic_NetKAT_Net.node): Yojson.Safe.json =
    match v with
    | Switch s_id -> 
        `Assoc [("type", `String "switch"); 
                ("id", `Intlit (Int64.to_string s_id))]
    | Host (dladdr, nwaddr) ->
        `Assoc [("type", `String "host");
                ("mac", `String (Frenetic_Packet.string_of_mac dladdr));
                ("ip", `String (Frenetic_Packet.string_of_ip nwaddr))] in

  let edge_to_json (e: Topo.edge) idmap = 
    let src, src_port = Topo.edge_src e in
    let dst, dst_port = Topo.edge_dst e in 
    let src = Topo.vertex_to_label t src in 
    let dst = Topo.vertex_to_label t dst in  

    let src_id = VertexMap.find_exn !idmap (Yojson.Basic.to_string (Yojson.Safe.to_basic (vertex_to_json src)))  in 
    let dst_id = VertexMap.find_exn !idmap (Yojson.Basic.to_string (Yojson.Safe.to_basic (vertex_to_json dst)))  in 
    `Assoc [("src_id", `Int src_id);
            ("src_port", `Int (Int32.to_int_exn src_port));
            ("label", `String "");
            ("dst_id", `Int dst_id);
            ("dst_port", `Int (Int32.to_int_exn dst_port))] in

  let vertices = `List (Topo.VertexSet.fold (Topo.vertexes t)
      ~f: (fun acc v -> (vertex_to_json (Topo.vertex_to_label t v))::acc)
      ~init: []) in

  let module VertexMap = Map.Make(String) in 
  let vlist = Yojson.Basic.Util.to_list (Yojson.Safe.to_basic vertices) in 
  let idmap = ref VertexMap.empty in 
  let _ = List.iteri vlist ~f: (fun index el-> idmap := VertexMap.add !idmap (Yojson.Basic.to_string el) index;) in  
  let edges = `List (Topo.fold_edges (fun e acc -> (edge_to_json e idmap)::acc) t []) in
  Yojson.Safe.to_string (`Assoc [("nodes", vertices); ("links", edges);])
