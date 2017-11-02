open Core

module Z = struct
  include Z
  let hash_fold_t state z =
    [%hash_fold: int] state (hash z)
end

type t = Q.t = {
    num: Z.t; (** Numerator. *)
    den: Z.t; (** Denominator, >= 0 *)
} [@@deriving hash]

include (Q : module type of Q with type t := t)

let t_of_sexp _ = failwith "not implemented"
let sexp_of_t _ = failwith "not implemented"
let pp = pp_print
