open! Core
open Probnetkat

type scheme =
  | SPF   (** no resilience *)
  | ECMP  (** with fast failover *)
  | RW    (** random walk *)
  | RRW   (** resilient random walk *)
  | F10
  [@@deriving sexp]


let uniform xs =
  let p = Prob.(1 // List.length xs) in
  List.map xs ~f:(fun x -> (x, p))


let run topo_file scheme dont_iterate fprob fbound cps show_fdd parallelize prism =
  let topo_name = Filename.chop_extension topo_file in
  let topo = Topology.parse topo_file in
  let sw_pol =
    let topo = Schemes.enrich_topo topo in
    match scheme with
    | SPF -> `Switchwise (Schemes.shortest_path topo topo_name)
    | ECMP -> `Switchwise (Schemes.resilient_ecmp topo topo_name)
    | RW -> `Switchwise (Schemes.random_walk topo)
    | RRW -> `Switchwise (Schemes.resilient_random_walk topo)
    | F10 -> `Portwise (Schemes.f10 topo topo_name)
  in
  let model =
    Model.make
      ~topo
      ~sw_pol
      ~failure_prob:(fun _ _ -> fprob)
      ~max_failures:fbound
      ~typ:`Legacy (* single destination: sw = Params.destination *)
      ()
  in
  if prism then
    let input_dist =
      Topology.ingress_locs ~dst:Params.destination topo
      |> List.map ~f:(fun (sw,pt) ->
        let sw = Topology.sw_val topo sw in
        [(Params.sw, sw); (Params.pt, pt)]
      )
      |> uniform
    in
    Prism.Code.of_pol model ~input_dist
    |> printf "%s\n%!";
    exit 0
  else
    let open Symbolic in
    Fdd.use_cps := cps;
    let bound = if dont_iterate then Some 0 else None in
    let fdd = Fdd.of_pol ~parallelize ~bound ~auto_order:true model in
    if show_fdd then Fdd.render fdd;
    let input_dist = Topology.uniform_ingress topo ~dst:Params.destination in
    let output_dist = Fdd.output_dist fdd ~input_dist in
    let avg_p = Packet.Dist.prob output_dist ~f:(fun pk ->
      Packet.test pk (Fdd.abstract_field Params.sw) Params.destination)
    in
    let avg_p' = Prob.to_q avg_p in
    Format.printf "E[delivered] = %a/%a\n%!"
      Z.pp_print (Q.num avg_p') Z.pp_print (Q.den avg_p');
    Format.printf "E[delivered] ≈ %f\n%!" (Prob.to_float avg_p)


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
    and cps = flag "cps" no_arg
      ~doc:" use CPS-style compilation strategy"
    and show_fdd = flag "show-fdd" no_arg
      ~doc:" render resulting FDD"
    and no_branch_par = flag "no-branch-par" no_arg
      ~doc:" don't parallelize disjoint branches"
    and prism = flag "prism" no_arg
      ~doc:" compile model to prism code"
    in
    fun () ->
      let fprob =
        match String.split fprob ~on:'/' with
        | [n; d] -> Prob.(Int.of_string n // Int.of_string d)
        | _ -> failwith "Flag '-fail-with' must be fraction of the form 'n/d'."
      in
      run topo scheme dont_iterate fprob fbound cps show_fdd (not no_branch_par) prism
  ]


let () = Command.run cmd
