open! Core
open Probnetkat

(* let topo_file = Sys.argv.(1)
let k = Int.of_string Sys.argv.(2) (* failure bound. -1 means unbounded *)
let fprob =
  let n,d = String.lsplit2_exn Sys.argv.(3) ~on:'/' in
  Prob.(Int.of_string n // Int.of_string d)
let topo_name = Filename.chop_extension topo_file *)

(* link failure probabilities *)
(* let failure_prob _sw _pt = fprob *)

(* Limit on maximum failures "encountered" by a packet. A packet encounters
   a failure if it occurs on a link that is incident to the current location
   of the packet, indepedently of whether the packet was planning to use that
   link or not. *)
(* let max_failures = if k<0 then None else Some k *)

type scheme =
  | ECMP
  | RECMP
  | SPF
  [@@deriving sexp]


let run topo_file scheme dont_iterate fprob fbound =
  let topo_name = Filename.chop_extension topo_file in
  let topo = Topology.parse topo_file in
  let topo' = Schemes.enrich_topo topo in
  let sw_pol = `Switchwise (Schemes.ecmp topo' topo_name) in
  printf "Hello, World!"

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Build & compile PNK model of given topology."
  [%map_open
    let topo = anon ("TOPOLOGY" %: file)
    and scheme = flag "scheme" (required (sexp_conv scheme_of_sexp))
      ~doc:" the routing scheme to be used."
    and dont_iterate = flag "dont-iterate" no_arg
      ~doc:" replace while loops with conditionals"
    and fprob = flag "fail-with" (required string)
      ~doc:" link failure probability (links fail iid)"
    and fbound = flag "fbound" (optional int)
      ~doc:" maximum number of link failures"
    in
    fun () ->
      let fprob =
        match String.split fprob ~on:'/' with
        | [n; d] -> Prob.(Int.of_string n // Int.of_string d)
        | _ -> failwith "Flag '-fail-with' must be fraction of the form 'n/d'."
      in
      run topo scheme dont_iterate fprob fbound
  ]


let () = Command.run cmd
