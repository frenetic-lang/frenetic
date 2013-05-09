open Equalities

module type HasLt = 
 functor (T:Typ) ->
 sig 
  
 end

module type HasLe = 
 functor (T:Typ) ->
 sig 
  
 end

module type EqLt = 
 sig 
  type t 
 end

module type EqLe = 
 sig 
  type t 
 end

module type EqLtLe = 
 sig 
  type t 
 end

module type LtNotation = 
 functor (E:EqLt) ->
 sig 
  
 end

module type LeNotation = 
 functor (E:EqLe) ->
 sig 
  
 end

module type LtLeNotation = 
 functor (E:EqLtLe) ->
 sig 
  
 end

module type EqLtNotation = 
 functor (E:EqLt) ->
 sig 
  
 end

module type EqLeNotation = 
 functor (E:EqLe) ->
 sig 
  
 end

module type EqLtLeNotation = 
 functor (E:EqLtLe) ->
 sig 
  
 end

module type EqLt' = 
 sig 
  type t 
 end

module type EqLe' = 
 sig 
  type t 
 end

module type EqLtLe' = 
 sig 
  type t 
 end

module type IsStrOrder = 
 functor (E:EqLt) ->
 sig 
  
 end

module type LeIsLtEq = 
 functor (E:EqLtLe') ->
 sig 
  
 end

module type StrOrder = 
 sig 
  type t 
 end

module type StrOrder' = 
 sig 
  type t 
 end

module type HasCmp = 
 functor (T:Typ) ->
 sig 
  val compare : T.t -> T.t -> int
 end

module type CmpNotation = 
 functor (T:Typ) ->
 functor (C:sig 
  val compare : T.t -> T.t -> int
 end) ->
 sig 
  
 end

module type CmpSpec = 
 functor (E:EqLt') ->
 functor (C:sig 
  val compare : E.t -> E.t -> int
 end) ->
 sig 
  
 end

module type HasCompare = 
 functor (E:EqLt) ->
 sig 
  val compare : E.t -> E.t -> int
 end

module type DecStrOrder = 
 sig 
  type t 
  
  val compare : t -> t -> int
 end

module type DecStrOrder' = 
 sig 
  type t 
  
  val compare : t -> t -> int
 end

module type OrderedType = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type OrderedType' = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type OrderedTypeFull = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type OrderedTypeFull' = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type UsualStrOrder = 
 sig 
  type t 
 end

module type UsualDecStrOrder = 
 sig 
  type t 
  
  val compare : t -> t -> int
 end

module type UsualOrderedType = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type UsualOrderedTypeFull = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type UsualStrOrder' = 
 sig 
  type t 
 end

module type UsualDecStrOrder' = 
 sig 
  type t 
  
  val compare : t -> t -> int
 end

module type UsualOrderedType' = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type UsualOrderedTypeFull' = 
 sig 
  type t 
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type LtIsTotal = 
 functor (E:EqLt') ->
 sig 
  
 end

module type TotalOrder = 
 sig 
  type t 
 end

module type UsualTotalOrder = 
 sig 
  type t 
 end

module type TotalOrder' = 
 sig 
  type t 
 end

module type UsualTotalOrder' = 
 sig 
  type t 
 end

module Compare2EqBool : 
 functor (O:DecStrOrder') ->
 sig 
  val eqb : O.t -> O.t -> bool
 end

module DSO_to_OT : 
 functor (O:DecStrOrder) ->
 sig 
  type t = O.t
  
  val compare : t -> t -> int
  
  val eqb : O.t -> O.t -> bool
  
  val eq_dec : O.t -> O.t -> bool
 end

module OT_to_Full : 
 functor (O:OrderedType') ->
 sig 
  type t = O.t
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module OTF_LtIsTotal : 
 functor (O:OrderedTypeFull') ->
 sig 
  
 end

module OTF_to_TotalOrder : 
 functor (O:OrderedTypeFull) ->
 sig 
  type t = O.t
  
  val compare : t -> t -> int
  
  val eq_dec : t -> t -> bool
 end

module type HasLeb = 
 functor (T:Typ) ->
 sig 
  val leb : T.t -> T.t -> bool
 end

module type HasLtb = 
 functor (T:Typ) ->
 sig 
  val ltb : T.t -> T.t -> bool
 end

module type LebNotation = 
 functor (T:Typ) ->
 functor (E:sig 
  val leb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type LtbNotation = 
 functor (T:Typ) ->
 functor (E:sig 
  val ltb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type LebSpec = 
 functor (T:Typ) ->
 functor (X:sig 
  
 end) ->
 functor (Y:sig 
  val leb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type LtbSpec = 
 functor (T:Typ) ->
 functor (X:sig 
  
 end) ->
 functor (Y:sig 
  val ltb : T.t -> T.t -> bool
 end) ->
 sig 
  
 end

module type LeBool = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type LtBool = 
 sig 
  type t 
  
  val ltb : t -> t -> bool
 end

module type LeBool' = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type LtBool' = 
 sig 
  type t 
  
  val ltb : t -> t -> bool
 end

module type LebIsTotal = 
 functor (X:LeBool') ->
 sig 
  
 end

module type TotalLeBool = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type TotalLeBool' = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type LebIsTransitive = 
 functor (X:LeBool') ->
 sig 
  
 end

module type TotalTransitiveLeBool = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type TotalTransitiveLeBool' = 
 sig 
  type t 
  
  val leb : t -> t -> bool
 end

module type HasBoolOrdFuns = 
 functor (T:Typ) ->
 sig 
  val eqb : T.t -> T.t -> bool
  
  val ltb : T.t -> T.t -> bool
  
  val leb : T.t -> T.t -> bool
 end

module type HasBoolOrdFuns' = 
 functor (T:Typ) ->
 sig 
  val eqb : T.t -> T.t -> bool
  
  val ltb : T.t -> T.t -> bool
  
  val leb : T.t -> T.t -> bool
 end

module type BoolOrdSpecs = 
 functor (O:EqLtLe) ->
 functor (F:sig 
  val eqb : O.t -> O.t -> bool
  
  val ltb : O.t -> O.t -> bool
  
  val leb : O.t -> O.t -> bool
 end) ->
 sig 
  
 end

module type OrderFunctions = 
 functor (E:EqLtLe) ->
 sig 
  val compare : E.t -> E.t -> int
  
  val eqb : E.t -> E.t -> bool
  
  val ltb : E.t -> E.t -> bool
  
  val leb : E.t -> E.t -> bool
 end

module type OrderFunctions' = 
 functor (E:EqLtLe) ->
 sig 
  val compare : E.t -> E.t -> int
  
  val eqb : E.t -> E.t -> bool
  
  val ltb : E.t -> E.t -> bool
  
  val leb : E.t -> E.t -> bool
 end

module OTF_to_TTLB : 
 functor (O:OrderedTypeFull') ->
 sig 
  val leb : O.t -> O.t -> bool
  
  type t = O.t
 end

module TTLB_to_OTF : 
 functor (O:TotalTransitiveLeBool') ->
 sig 
  type t = O.t
  
  val compare : O.t -> O.t -> int
  
  val eqb : O.t -> O.t -> bool
  
  val eq_dec : O.t -> O.t -> bool
 end

