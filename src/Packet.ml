open Core

module Field = Fdd.Field
module Value = Fdd.Value

type nomval =
  | Const of Value.t
  | Atom (** An atom in the sense of nominal sets. Some fixed that value that is different
             from all constants. To a first approximation, a sort of wildcard, but it ranges
             only over values that do not appear as constants.
          *)
  [@@deriving compare, sexp]

type pk = nomval Field.Map.t
(** Symbolic packet. Represents a set of concrete packets { π }.

    f |-> Const v  means π.f = v
    f |-> Atom     means π.f \in { values not appearing as f-values }
    f |-> ⊥        means π.f can have any value

    In particular, the empty map represents the set of all packets, and a map
    that associates a constant with every field represents a singleton set.
*)

(* let test (f : field) (v : nomval) (pk : pk) : bool =
  Option.value_map (Map.find pk f) ~f:(Value.equal v) ~default:false *)

let modify (f : Field.t) (v : nomval) (pk : pk) : pk =
  Map.add pk ~key:f ~data:v

let pp fmt pk =
  Format.fprintf fmt "@[";
  if Map.is_empty pk then Format.fprintf fmt "*@ " else
  Map.iteri pk ~f:(fun ~key ~data -> Format.fprintf fmt "@[%s=%d@]@ " key data);
  Format.fprintf fmt "@]";
  ()
