open Bool

module type Nop = 
 sig 
  
 end

module type Typ = 
 sig 
  type t 
 end

module type HasEq = 
 functor (T:Typ) ->
 sig 
  
 end

module type Eq = 
 sig 
  type t 
 end

module type EqNotation = 
 functor (E:Eq) ->
 sig 
  
 end

module type Eq' = 
 sig 
  type t 
 end

module type IsEq = 
 functor (E:Eq) ->
 sig 
  
 end

module type IsEqOrig = 
 functor (E:Eq') ->
 sig 
  
 end

module type HasEqDec = 
 functor (E:Eq') ->
 sig 
  val eq_dec : E.t -> E.t -> bool
 end

module type HasEqb = 
 functor (T:Typ) ->
 sig 
  val eqb : T.t -> T.t -> bool
 end

module type EqbSpec = 
 functor (T:Typ) ->
 functor (X:sig 
  
 end) ->
 functor (Y:sig 
  val eqb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type EqbNotation = 
 functor (T:Typ) ->
 functor (E:sig 
  val eqb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type HasEqBool = 
 functor (E:Eq) ->
 sig 
  val eqb : E.t -> E.t -> bool
 end

module type EqualityType = 
 sig 
  type t 
 end

module type EqualityTypeOrig = 
 sig 
  type t 
 end

module type EqualityTypeBoth = 
 sig 
  type t 
 end

module type DecidableType = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type DecidableTypeOrig = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type DecidableTypeBoth = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type BooleanEqualityType = 
 sig 
  type t 
  
  val eqb : t -> t -> bool
 end

module type BooleanDecidableType = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
  
  val eqb : t -> t -> bool
 end

module type DecidableTypeFull = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
  
  val eqb : t -> t -> bool
 end

module type EqualityType' = 
 sig 
  type t 
 end

module type EqualityTypeOrig' = 
 sig 
  type t 
 end

module type EqualityTypeBoth' = 
 sig 
  type t 
 end

module type DecidableType' = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type DecidableTypeOrig' = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type DecidableTypeBoth' = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type BooleanEqualityType' = 
 sig 
  type t 
  
  val eqb : t -> t -> bool
 end

module type BooleanDecidableType' = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
  
  val eqb : t -> t -> bool
 end

module type DecidableTypeFull' = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
  
  val eqb : t -> t -> bool
 end

module BackportEq = 
 functor (E:Eq) ->
 functor (F:sig 
  
 end) ->
 struct 
  
 end

module UpdateEq = 
 functor (E:Eq) ->
 functor (F:sig 
  
 end) ->
 struct 
  
 end

module Backport_ET = 
 functor (E:EqualityType) ->
 struct 
  type t = E.t
 end

module Update_ET = 
 functor (E:EqualityTypeOrig) ->
 struct 
  type t = E.t
 end

module Backport_DT = 
 functor (E:DecidableType) ->
 struct 
  type t = E.t
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec =
    E.eq_dec
 end

module Update_DT = 
 functor (E:DecidableTypeOrig) ->
 struct 
  type t = E.t
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec =
    E.eq_dec
 end

module HasEqDec2Bool = 
 functor (E:Eq) ->
 functor (F:sig 
  val eq_dec : E.t -> E.t -> bool
 end) ->
 struct 
  (** val eqb : E.t -> E.t -> bool **)
  
  let eqb x y =
    if F.eq_dec x y then true else false
 end

module HasEqBool2Dec = 
 functor (E:Eq) ->
 functor (F:sig 
  val eqb : E.t -> E.t -> bool
 end) ->
 struct 
  (** val eq_dec : E.t -> E.t -> bool **)
  
  let eq_dec x y =
    let b = F.eqb x y in if b then true else false
 end

module Dec2Bool = 
 functor (E:DecidableType) ->
 struct 
  type t = E.t
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec =
    E.eq_dec
  
  (** val eqb : E.t -> E.t -> bool **)
  
  let eqb x y =
    if eq_dec x y then true else false
 end

module Bool2Dec = 
 functor (E:BooleanEqualityType) ->
 struct 
  type t = E.t
  
  (** val eqb : t -> t -> bool **)
  
  let eqb =
    E.eqb
  
  (** val eq_dec : E.t -> E.t -> bool **)
  
  let eq_dec x y =
    let b = E.eqb x y in if b then true else false
 end

module BoolEqualityFacts = 
 functor (E:BooleanEqualityType') ->
 struct 
  (** val eqb_spec : E.t -> E.t -> reflect **)
  
  let eqb_spec x y =
    iff_reflect (E.eqb x y)
 end

module type HasUsualEq = 
 functor (T:Typ) ->
 sig 
  
 end

module type UsualEq = 
 sig 
  type t 
 end

module type UsualIsEq = 
 functor (E:UsualEq) ->
 sig 
  
 end

module type UsualIsEqOrig = 
 functor (E:UsualEq) ->
 sig 
  
 end

module type UsualEqualityType = 
 sig 
  type t 
 end

module type UsualDecidableType = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type UsualDecidableTypeOrig = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type UsualDecidableTypeBoth = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module type UsualBoolEq = 
 sig 
  type t 
  
  val eqb : t -> t -> bool
 end

module type UsualDecidableTypeFull = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
  
  val eqb : t -> t -> bool
 end

module type MiniDecidableType = 
 sig 
  type t 
  
  val eq_dec : t -> t -> bool
 end

module Make_UDT = 
 functor (M:MiniDecidableType) ->
 struct 
  type t = M.t
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec =
    M.eq_dec
 end

module Make_UDTF = 
 functor (M:UsualBoolEq) ->
 struct 
  type t = M.t
  
  (** val eqb : t -> t -> bool **)
  
  let eqb =
    M.eqb
  
  (** val eq_dec : M.t -> M.t -> bool **)
  
  let eq_dec x y =
    let b = M.eqb x y in if b then true else false
 end

