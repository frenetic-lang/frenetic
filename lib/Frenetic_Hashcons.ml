open Core.Std

module type HASHTYPE = sig
  type t
  val get : t -> int
  val unget : int -> t
  val clear : Core.Std.Int.Set.t -> unit
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

  let clear (preserve : Int.Set.t) : unit =
    let max_key = ref 0 in
    T.iter tbl ~f:(fun ~key ~data ->
      max_key := max !max_key data;
      if not (Int.Set.mem preserve data) then begin
        U.remove untbl data;
        T.remove tbl key
      end);
    idx := !max_key + 1

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