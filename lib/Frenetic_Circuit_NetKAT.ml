open Core.Std
open Frenetic_NetKAT

type channel = int
type loc = switchId * portId
type hop = switchId * portId * switchId * portId
type circuit = { source : loc
               ; sink : loc
               ; path : hop list
               ; channel : channel
               }

type config = circuit list
let (>>=) = Result.(>>=)

let flatten_seqs pol =
  let rec aux p acc = match p with
    | Seq(p1, p2) -> (aux p1 (aux p2 acc))
    | _ as p -> p::acc in
  aux pol []

let flatten_unions pol =
  let rec aux p acc = match p with
    | Union(p1, p2) -> (aux p1 (aux p2 acc))
    | _ as p -> p::acc in
  aux pol []

let string_of_policy_list (pols: policy list) (sep:string) =
  String.concat ~sep:sep (List.map pols (Frenetic_NetKAT_Pretty.string_of_policy))

let source_of_filter (filter:pred) : (loc, string) Result.t = match filter with
  | And( Test( Switch swid ),
         Test( Location( Physical ptid)))
  | And( Test( Location( Physical ptid)),
         Test( Switch swid )) ->
    Ok (swid, ptid)
  | _ -> Error "Circuit source must filter on both switch and port"

let path_of_policies (pol:policy list) : ((hop list * portId), string) Result.t =
  let rec aux pol_list hops = match pol_list with
    | Mod( Location( Physical pt))::[] ->
      Ok (hops, pt)
    | Link(s,p,s',p')::rest ->
      let hop = (s,p,s',p') in
      aux rest (hop::hops)
    | ps -> Error ( sprintf
                  "Circuit be a sequence of links ending in a port assignment, but is \n%s\n"
                  (string_of_policy_list ps "; ")) in
  aux pol []

let circuit_of_policy (pol:policy) : (circuit, string) Result.t =
  let pols = flatten_seqs pol in
  match pols with
  | (Filter f)::rest ->
    source_of_filter f >>= fun source ->
    begin match rest with
    | Mod( Channel ch )::path ->
      let channel = ch in
      path_of_policies path >>= fun (path, port) ->
      let sw,_ = source in
      let sink = match (List.last path) with
        | Some (_,_,sw',_) -> (sw', port)
        | None             -> (sw, port) in
      Ok { source ; sink ; path ; channel }
    | _ -> Error "Circuit must begin with a channel assignment" end
  | ps -> Error (sprintf "Circuit must sequence a filter with a path, but is \n%s\n"
                   (string_of_policy_list ps "; "))

let config_of_policy (pol:policy) : (config, string) Result.t =
  let pols = flatten_unions pol in
  List.fold pols ~init:(Ok []) ~f:(fun acc pol ->
      match acc, circuit_of_policy pol with
      | Ok circs, Ok circuit -> Ok (circuit::circs)
      | Ok _, Error e -> Error e
      | Error e, Ok _ -> Error e
      | Error es, Error e -> Error (String.concat ~sep:"\n" [es; e]))

let string_of_loc ((sw,pt):loc) =
  sprintf "Switch: %Ld Port:%ld" sw pt

let string_of_hop (h:hop) : string =
  let sw, pt, sw', pt' = h in
  sprintf "%Ld:%ld => %Ld:%ld" sw pt sw' pt'

let string_of_circuit (c) : string =
  let paths = String.concat ~sep:" | " (List.map c.path ~f:string_of_hop) in
  sprintf "Source: %s\n Sink: %s \n Path: %s\n Channel: %d\n"
    (string_of_loc c.source)
    (string_of_loc c.sink)
    (paths)
    c.channel

let string_of_config (c:config) : string =
  String.concat ~sep:"\n" (List.map c ~f:string_of_circuit)
