open Core


module Make(V:Vlr.Value)(L:Vlr.Lattice)(R:Vlr.Result) = struct
  include Vlr.Make(V)(L)(R)

  type bin_t = {
    root : bin_d;
    graph : bin_d Int.Table.t;
  }
  and bin_d =
    | BLeaf of Sexp.t
    | BBranch of {
      test : V.t * L.t;
      tru : int;
      fls : int;
    }
  [@@deriving bin_io]

  let to_bin (t : t) : bin_t =
    let graph = Int.Table.create () in
    let rec convert (t : t) : bin_d =
      match unget t with
      | Leaf r -> BLeaf (R.sexp_of_t r)
      | Branch { test; tru=tru_t; fls=fls_t } ->
        let tru = (tru_t :> int) in
        let fls = (fls_t :> int) in
        Hashtbl.add graph ~key:tru ~data:(convert tru_t) |> ignore;
        Hashtbl.add graph ~key:fls ~data:(convert fls_t) |> ignore;
        BBranch { test; tru; fls }
    in
    let root = convert t in
    { root; graph }

  let of_bin { root; graph } : t =
    let rec of_bin_d = function
      | BLeaf sexp -> const (R.t_of_sexp sexp)
      | BBranch { test; tru; fls} ->
        let tru = Hashtbl.find_exn graph tru |> of_bin_d in
        let fls = Hashtbl.find_exn graph fls |> of_bin_d in
        unchecked_cond test tru fls
    in
    of_bin_d root


end
