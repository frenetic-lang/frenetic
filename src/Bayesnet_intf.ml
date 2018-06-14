(* Bayesian networks. Can represent joint distributions compactly, provided
   they have some structure that permits factorization.
*)

open Core

module type S = sig
  (* type of random variables *)
  module type VAR = sig
    type t
    include Vlr.HashCmp with type t := t
    include Comparable.S with type t := t
  end

  (* values that the random variables range over *)
  module type VAL = sig
    type t
    include Vlr.HashCmp with type t := t
  end

  module type BAYESNET = sig
    module Var : VAR
    module Val : VAL
    module Dist : Dist_intf.S with module Dom = Val
    type t

    (** {2} basic properties *)
    val dom : t -> Set.M(Var).t

    (** {2} building bayesian networks *)
    val empty : t
    val add : t -> var:Var.t -> dist:Dist.t -> t
    val remove : t -> var:Var.t -> [`Ok of t | `Not_present ]
    val remove_exn : t -> var:Var.t -> t


    (** {2} composing bayesian networks *)

    (** independent product of networks of disjoint sets of variables *)
    val indep_prod : t -> t -> [`Ok of t | `Clash of Var.t ]
    val indep_prod_exn : t -> t -> t

    (** convex combination of two networks over the same domain *)
    val convex_sum : t -> t -> p:Prob.t -> t

  end
end

