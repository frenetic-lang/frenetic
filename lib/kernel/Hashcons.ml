open Core

module type HASHTYPE = sig
  type t
  val get : t -> int
  val unget : int -> t
  val clear : Int.Set.t -> unit
end

module Make (Value : Hashtbl.Key) : HASHTYPE with type t = Value.t = struct

  module T = Hashtbl.Make (Value)
  (* TODO(arjun): Since these are allocated contiguously, it would be
     better to use a growable array ArrayList<Int> *)
  (* TODO(jnf): are you suggesting we port Frentic to Java?! *)
  module U = Int.Table

  type t = Value.t

  let tbl : int T.t = T.create ~size:1000 ()
  let untbl : t U.t = U.create ~size:1000 ()

  let idx = ref 0

  let clear (preserve : Int.Set.t) : unit = begin
    (* SJS: iterate over _copy_ of tbl to avoid side effect hell! *)
    T.to_alist tbl
      |> List.filter ~f:(fun (_,v) -> not (Set.mem preserve v))
      |> List.iter ~f:(fun (k,v) -> T.remove tbl k; U.remove untbl v);
    idx := 1 + T.fold tbl ~init:0 ~f:(fun ~key ~data -> max data)
  end


  let gensym () =
    let r = !idx in
    idx := !idx + 1;
    r

  let get (v : t) =
    T.find_or_add tbl v ~default:(fun () ->
      let n = gensym () in
      U.add_exn untbl n v;
      n)

  let unget (idx : int) : t = U.find_exn untbl idx

end
