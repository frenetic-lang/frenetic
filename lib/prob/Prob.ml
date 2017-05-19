open Core.Std

include Q

let t_of_sexp _ = failwith "not implemented"
let sexp_of_t _ = failwith "not implemented"
let pp = pp_print

(* temporary hack, until this has been fixed upstream *)
let to_float q =
  let num = Z.to_float (Q.num q) in
  let den = Z.to_float (Q.den q) in
  Float.(num / den)
