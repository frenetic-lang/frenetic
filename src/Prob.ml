open Core

module Z = struct
  include Z
  let hash_fold_t state z =
    [%hash_fold: int] state (hash z)

  let t_of_sexp x =
    String.t_of_sexp x
    |> of_string

  let sexp_of_t t =
    to_string t
    |> String.sexp_of_t
end

type t = Q.t = {
    num: Z.t; (** Numerator. *)
    den: Z.t; (** Denominator, >= 0 *)
} [@@deriving hash, sexp]

include (Q : module type of Q with type t := t)

let pp fmt t =
  if Z.(t.den <= of_int 100) then
    pp_print fmt t
  else
    Float.pp fmt (to_float t)

let to_string t =
  if Z.(t.den <= of_int 100) then
    to_string t
  else
    sprintf "%.2g" (to_float t)
