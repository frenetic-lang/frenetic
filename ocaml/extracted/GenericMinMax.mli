open Datatypes
open Orders
open OrdersTac

type __ = Obj.t

module type HasMax = 
 functor (E:EqLe') ->
 sig 
  val max : E.t -> E.t -> E.t
 end

module type HasMin = 
 functor (E:EqLe') ->
 sig 
  val min : E.t -> E.t -> E.t
 end

module type HasMinMax = 
 functor (E:EqLe) ->
 sig 
  val max : E.t -> E.t -> E.t
  
  val min : E.t -> E.t -> E.t
 end

val gmax : ('a1 -> 'a1 -> int) -> 'a1 -> 'a1 -> 'a1

val gmin : ('a1 -> 'a1 -> int) -> 'a1 -> 'a1 -> 'a1

module GenericMinMax : 
 functor (O:OrderedTypeFull') ->
 sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end

module MaxLogicalProperties : 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 sig 
  module Private_Tac : 
   sig 
    
   end
 end

module MinMaxLogicalProperties : 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Rev : 
   sig 
    module ORev : 
     sig 
      type t = O.t
     end
    
    module MRev : 
     sig 
      val max : O.t -> O.t -> O.t
     end
    
    module MPRev : 
     sig 
      module Private_Tac : 
       sig 
        
       end
     end
   end
 end

module MinMaxDecProperties : 
 functor (O:OrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  val max_case_strong :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
    'a1) -> 'a1
  
  val max_case :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : O.t -> O.t -> bool
  
  val min_case_strong :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
    'a1) -> 'a1
  
  val min_case :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : O.t -> O.t -> bool
 end

module MinMaxProperties : 
 functor (O:OrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  module OT : 
   sig 
    type t = O.t
    
    val compare : t -> t -> int
    
    val eq_dec : t -> t -> bool
   end
  
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Rev : 
   sig 
    module ORev : 
     sig 
      type t = O.t
     end
    
    module MRev : 
     sig 
      val max : O.t -> O.t -> O.t
     end
    
    module MPRev : 
     sig 
      module Private_Tac : 
       sig 
        
       end
     end
   end
  
  val max_case_strong :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
    'a1) -> 'a1
  
  val max_case :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : O.t -> O.t -> bool
  
  val min_case_strong :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
    'a1) -> 'a1
  
  val min_case :
    O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : O.t -> O.t -> bool
 end

module UsualMinMaxLogicalProperties : 
 functor (O:UsualTotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Rev : 
   sig 
    module ORev : 
     sig 
      type t = O.t
     end
    
    module MRev : 
     sig 
      val max : O.t -> O.t -> O.t
     end
    
    module MPRev : 
     sig 
      module Private_Tac : 
       sig 
        
       end
     end
   end
 end

module UsualMinMaxDecProperties : 
 functor (O:UsualOrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  module Private_Dec : 
   sig 
    val max_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val max_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val max_dec : O.t -> O.t -> bool
    
    val min_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val min_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val min_dec : O.t -> O.t -> bool
   end
  
  val max_case_strong : O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val max_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : O.t -> O.t -> bool
  
  val min_case_strong : O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val min_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : O.t -> O.t -> bool
 end

module UsualMinMaxProperties : 
 functor (O:UsualOrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 sig 
  module OT : 
   sig 
    type t = O.t
    
    val compare : t -> t -> int
    
    val eq_dec : t -> t -> bool
   end
  
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Rev : 
   sig 
    module ORev : 
     sig 
      type t = O.t
     end
    
    module MRev : 
     sig 
      val max : O.t -> O.t -> O.t
     end
    
    module MPRev : 
     sig 
      module Private_Tac : 
       sig 
        
       end
     end
   end
  
  module Private_Dec : 
   sig 
    val max_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val max_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val max_dec : O.t -> O.t -> bool
    
    val min_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val min_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val min_dec : O.t -> O.t -> bool
   end
  
  val max_case_strong : O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val max_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : O.t -> O.t -> bool
  
  val min_case_strong : O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val min_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : O.t -> O.t -> bool
 end

module TOMaxEqDec_to_Compare : 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 functor (E:sig 
  val eq_dec : O.t -> O.t -> bool
 end) ->
 sig 
  val compare : O.t -> O.t -> int
 end

module TOMaxEqDec_to_OTF : 
 functor (O:TotalOrder) ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 functor (E:sig 
  val eq_dec : O.t -> O.t -> bool
 end) ->
 sig 
  type t = O.t
  
  val eq_dec : O.t -> O.t -> bool
  
  val compare : O.t -> O.t -> int
 end

