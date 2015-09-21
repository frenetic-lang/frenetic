(* TODO: SJS: use Core.Std (Hashable, ...) *)

module type HASHABLE = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module type HASHTYPE = sig
  type value
  val get : value -> int
  val unget : int -> value
  val clear : Core.Std.Int.Set.t -> unit
end

module Make (Value : HASHABLE) : HASHTYPE with type value = Value.t = struct

  module T = Hashtbl.Make (Value)
  (* TODO(arjun): Since these are allocated contiguously, it would be
     better to use a growable array ArrayList<Int> *)
  (* TODO(jnf): are you suggesting we port Frentic to Java?! *)
  module U = Hashtbl.Make(struct
    type t = int
    let hash n = n
    let equal x y = x = y
  end)

  type value = Value.t

  let tbl : int T.t = T.create 1000
  let untbl : value U.t = U.create 1000

  let idx = ref 0

  let clear (preserve : Core.Std.Int.Set.t) : unit =
    let open Core.Std in
    let max_key = ref 0 in
    T.iter (fun key data ->
      max_key := max !max_key data;
      if Int.Set.mem preserve data then
        ()
      else
        (U.remove untbl data;
         T.remove tbl key))
      tbl;
    idx := !max_key + 1

  let gensym () =
    let r = !idx in
    idx := !idx + 1;
    r

  let get (v : value) =
    try
      T.find tbl v
    with Not_found ->
      begin
        let n = gensym () in
        T.add tbl v n;
        U.add untbl n v;
        n
      end

  let unget (idx : int) : value = U.find untbl idx

end