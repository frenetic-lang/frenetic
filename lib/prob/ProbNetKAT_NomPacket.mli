open ProbNetKAT
open ProbNetKAT_Packet

type 'domain_witness hyperpoint = private int list
type 'domain_witness codepoint = private int
type 'domain_witness index = private { i : int }

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
    val to_index : t -> int (** non-negative matrix index *)
    val of_index : int -> t (** unsafe! *)
  end

  and Index : sig
    type t = domain_witness index
    val max : t
    val of_pk : pk -> t
    val to_pk : t -> pk
    val test : Field.t -> Value.t -> t -> bool
    val modify : Field.t -> Value.t -> t -> t
  end
end


module Make(D : ProbNetKAT.Domain) : S
