open Core.Std

module type OUTCOME = sig
  type t
  include Map.Key with type t := t
  val zero : t
  val one : t
  val union : t -> t -> t
  val seq : t -> t -> t
end

module Make(Outcome : OUTCOME) = struct
  type outcome = Outcome.t
  module M = Map.Make(Outcome)

  (* Invariant: values sum up to 1.0 *)
  type t = float M.t

  let hash t =
    M.to_alist t |> List.map ~f:(fun (o,p) -> (Hashtbl.hash o, p))

  let equal = M.equal Float.equal
  let compare = M.compare_direct Float.compare

  let dirac a = M.singleton a 1.0
  let zero = dirac Outcome.zero
  let one = dirac Outcome.one

  let convolution t1 t2 ~(op:outcome -> outcome -> outcome) : t =
    M.fold t1 ~init:M.empty ~f:(fun ~key:o1 ~data:p1 acc ->
      M.fold t2 ~init:acc ~f:(fun ~key:o2 ~data:p2 acc ->
        let o = op o1 o2 in
        let p = p1 *. p2 in
        M.change acc o (function
          | None -> Some p
          | Some p' -> Some (p' +. p))))

  let union = convolution ~op:Outcome.union
  let seq = convolution ~op:Outcome.seq

  (* This is the correct ProbNetKAT star semantics: 
     p* = 1 + p0 + p0;p1 + p0;p1;p2 + ... *)
  let star t =
    let rec loop acc power =
      let acc' = union acc power in
      if equal acc acc' 
        then acc
        else loop acc' (seq power t)
    in loop one t

end