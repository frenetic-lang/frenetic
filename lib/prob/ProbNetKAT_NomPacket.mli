open ProbNetKAT

type t = value Field.Map.t
type pk = t

type 'domain_witness hyperpoint = private int list
type 'domain_witness codepoint = private int

module type S = sig
  type domain_witness

  module rec Hyperpoint : sig
    type t = domain_witness hyperpoint
    val dimension : int list
    val to_codepoint : t -> Codepoint.t
    val of_codepoint : Codepoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
  end

  and Codepoint : sig
    type t = domain_witness codepoint
    val max : t
    val to_hyperpoint : t -> Hyperpoint.t
    val of_hyperpoint : Hyperpoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
    val to_idx : t -> int (** non-negative matrix index *)
  end
end


module Make(D : ProbNetKAT.Domain) : S
