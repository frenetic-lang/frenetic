open Orders

type ord =
| OEQ
| OLT
| OLE

val ord_rect : 'a1 -> 'a1 -> 'a1 -> ord -> 'a1

val ord_rec : 'a1 -> 'a1 -> 'a1 -> ord -> 'a1

val trans_ord : ord -> ord -> ord

module OrderFacts : 
 functor (O:TotalOrder') ->
 sig 
  
 end

module MakeOrderTac : 
 functor (O:TotalOrder') ->
 sig 
  
 end

module OTF_to_OrderTac : 
 functor (OTF:OrderedTypeFull) ->
 sig 
  module TO : 
   sig 
    type t = OTF.t
    
    val compare : t -> t -> int
    
    val eq_dec : t -> t -> bool
   end
 end

module OT_to_OrderTac : 
 functor (OT:OrderedType) ->
 sig 
  module OTF : 
   sig 
    type t = OT.t
    
    val compare : t -> t -> int
    
    val eq_dec : t -> t -> bool
   end
  
  module TO : 
   sig 
    type t = OTF.t
    
    val compare : t -> t -> int
    
    val eq_dec : t -> t -> bool
   end
 end

module TotalOrderRev : 
 functor (O:TotalOrder) ->
 sig 
  type t = O.t
 end

