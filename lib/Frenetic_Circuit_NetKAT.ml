open Core.Std
open Frenetic_NetKAT

type channel = int [@@deriving sexp,compare]
type loc = switchId * portId [@@deriving sexp,compare]
type hop = switchId * portId * switchId * portId
type circuit = { source : loc
               ; sink : loc
               ; path : hop list
               ; channel : channel
               }

type config = circuit list
let (>>=) = Result.(>>=)

(** Utility functions *)
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

let union = Frenetic_NetKAT_Optimize.mk_big_union
let seq = Frenetic_NetKAT_Optimize.mk_big_seq

let string_of_policy_list (pols: policy list) (sep:string) =
  String.concat ~sep:sep (List.map pols (Frenetic_NetKAT_Pretty.string_of_policy))

(** Generate circuit types from policies *)
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
      Ok (List.rev hops, pt)
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

(** Generate policies from circuit types *)
let policy_of_circuit (c:circuit) : policy =
  let pred = Filter( And( Test( Switch ( fst c.source )),
                          Test( Location( Physical( snd c.source ))))) in
  let channel = Mod( Channel c.channel ) in
  let path = List.fold c.path ~init:[channel; pred] ~f:(fun acc (sw,pt,sw',pt') ->
      let fwd = Mod( Location( Physical pt )) in
      let link = Link(sw,pt,sw',pt') in
      link::fwd::acc) in
  let out = Mod( Location( Physical( snd c.sink ))) in
  seq (List.rev (out::path))

let policy_of_config (c:config) : policy =
  let paths = List.map c ~f:policy_of_circuit in
  union paths

(** Pretty printing *)
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

(** Validation routines *)
type chan_config = { chan : channel
                   (* Forwarded to a port *)
                   ; incoming : portId option
                   (* Forwarded from a port *)
                   ; outgoing : portId option
                   }

let blank_chan = { chan = 0
                 ; incoming = None
                 ; outgoing = None
                 }

module ChannelTable = Hashtbl.Make( struct
    type t = channel [@@deriving sexp,compare]
    let compare = Int.compare
    let hash = Hashtbl.hash
  end )

type port_config = chan_config ChannelTable.t

module LocTable = Hashtbl.Make( struct
    type t = loc [@@deriving sexp,compare]
    let compare = compare_loc
    let hash = Hashtbl.hash
  end )

let validate_circuit (tbl:port_config LocTable.t) (c:circuit)
  : (circuit, string) Result.t =

  let loc_triples = List.fold c.path ~init:[(None,c.source,None)]
      ~f:(fun (prev::rest) (sw,pt,sw',pt') ->
          let (l1,l2,l3) = prev in
          match (l1,l2,l3) with
          | _,_,None ->
            let prev' = (l1,l2,Some(sw,pt)) in
            let src = (Some l2,(sw,pt),Some(sw',pt')) in
            let dst = (Some(sw,pt),(sw',pt'),None) in
            dst::src::prev'::rest
          | _ -> failwith "Malformed triple fold") in
  let (l1,l2,l3) = List.hd_exn loc_triples in
  let loc_triples = List.rev
      ((Some l2,c.sink,None)::(l1,l2,Some c.sink)::(List.tl_exn loc_triples)) in


  let update_incoming (conf:chan_config) this next =
    match conf.incoming with
    | None -> Ok { conf with incoming = Some (snd next) }
    | Some pt->
      if pt = snd next then Ok conf
      else
        Error( sprintf "Channel %d from %Ld:%ld is split between ports %ld and %ld"
                 conf.chan (fst this) (snd this) (snd next) pt) in

  let update_outgoing conf this prev =
    match conf.outgoing with
    | None ->
      Ok { conf with outgoing = Some (snd prev) }
    | Some pt ->
      if pt = snd prev then Ok conf
      else
        Error( sprintf "Channel %d to %Ld:%ld is merged from ports %ld and %ld"
                 conf.chan (fst this) (snd this) (snd prev) pt) in

  let update (conf:chan_config) (prev_opt, this, next_opt) =
    match prev_opt, next_opt with
    | Some prev, Some next ->
      if (fst prev) = (fst this)
      then update_outgoing conf this prev
      else if (fst this) = (fst next)
      then update_incoming conf this next
      else Error( sprintf "Invalid hops surrounding %Ld:%ld"
                    (fst this) (snd this))
    | None, Some next ->
      if (fst this) = (fst next)
      then update_incoming conf this next
      else Error( sprintf "Invalid hops surrounding %Ld:%ld"
                  (fst this) (snd this))
    | Some prev, None ->
      if (fst prev) = (fst this)
      then update_outgoing conf this prev
      else Error( sprintf "Invalid hops surrounding %Ld:%ld"
                    (fst this) (snd this))
    | None, None ->
      Error( sprintf "Invalid blank locations surrounding %Ld:%ld"
                    (fst this) (snd this))
  in

  let errors = List.fold loc_triples ~init:[] ~f:(fun errors triple ->
      let _,this,_ = triple in
      match LocTable.find tbl this with
      | None ->
        let tbl' = ChannelTable.create () in
        let es = begin match update { blank_chan with chan = c.channel } triple with
          | Ok conf -> ChannelTable.add_exn tbl' c.channel conf; errors
          | Error e -> e::errors end in
        LocTable.add_exn tbl this tbl';
        es
      | Some tbl' ->
        begin match ChannelTable.find tbl' c.channel with
          | None ->
            begin match update { blank_chan with chan = c.channel } triple with
              | Ok conf -> ChannelTable.add_exn tbl' c.channel conf; errors
              | Error e -> e::errors end
          | Some conf ->
            begin match update conf triple with
              | Ok conf -> ChannelTable.set tbl' c.channel conf; errors
              | Error e -> e::errors end
        end ) in
  match errors with
  | [] -> Ok c
  | es -> Error( String.concat ~sep:"\n" es)

let validate_config (c:config) : (config, string) Result.t =
  let tbl = LocTable.create ~size:(List.length c) () in
  List.fold c ~init:(Ok []) ~f:(fun acc circuit ->
      match acc, validate_circuit tbl circuit with
      | Ok circs, Ok circuit -> print_endline "Fine";Ok (circuit::circs)
      | Ok _, Error e -> printf "%s\n%!" (string_of_circuit circuit); Error e
      | Error e, Ok _ -> printf "%s\n%!" (string_of_circuit circuit); Error e
      | Error es, Error e -> Error (String.concat ~sep:"\n" [es; e]))
