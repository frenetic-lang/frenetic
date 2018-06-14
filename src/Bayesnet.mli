include Bayesnet_intf.S

(* a bayesian network with variables of type 'var taking on values of type 'value *)
type ('var, 'value) t

module Make : functor (Var : VAR) -> functor (Val : VAL) -> BAYESNET
  with module Var = Var
  with module Val = Val
  with type t = (Var.t, Val.t) t
