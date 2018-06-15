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

    (** [Missing_var x] is thrown by operations when an operation performed on
        a net [t] requires [x], but [x] is not in [vars t]. *)
    exception Missing_var of Var.t

    (** {2} basic properties *)
    val vars : t -> Set.M(Var).t
    val mass : t -> Prob.t

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

    (** [split_equal t (x,v)] splits [t] into two networks [(t_eq, t_neq)] in
        the following way:
          * t = t_eq + t_neq
          * mass(t_eq) = Pr(x=v) in t = Pr(x=v) in t_eq
          * mass(t_neq) = Pr(x!=v) in t = Pr(x!=v) in t_neq
    *)
    val split_equal : t -> Var.t * Val.t -> t * t

    val normalize : t -> t option



    (** {2} misc *)

    (** a [valuation] assigns values to all variables of a bayesian network *)
    type valuation = Val.t Map.M(Var).t

    (** fold over all possible valuations of the network.
        THIS OPERATION IS EXPONENTIAL! Avoid using this whenever possible. *)
    val fold : t -> init:'accum -> f:('accum -> Prob.t -> valuation -> 'accum) -> 'accum

  end
end

