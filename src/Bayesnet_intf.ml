(* Bayesian networks. Can represent joint distributions compactly, provided
   they have some structure that permits factorization.
*)

module type S = sig
  (* type of random variables *)
  module type VAR = sig
    type t
    include Vlr.HashCmp with type t := t
  end

  (* values that the random variables range over *)
  module type VAL = sig
    type t
  end

  module type BAYESNET = sig
    module Var : VAR
    module Val : VAL
    type t


  end
end

