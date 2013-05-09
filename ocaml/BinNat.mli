open BinNums
open BinPos
open Bool
open Datatypes
open OrdersTac
open Peano

type __ = Obj.t

module N : 
 sig 
  type t = coq_N
  
  val zero : coq_N
  
  val one : coq_N
  
  val two : coq_N
  
  val succ_double : coq_N -> coq_N
  
  val double : coq_N -> coq_N
  
  val succ : coq_N -> coq_N
  
  val pred : coq_N -> coq_N
  
  val succ_pos : coq_N -> positive
  
  val add : coq_N -> coq_N -> coq_N
  
  val sub : coq_N -> coq_N -> coq_N
  
  val mul : coq_N -> coq_N -> coq_N
  
  val compare : coq_N -> coq_N -> int
  
  val eqb : coq_N -> coq_N -> bool
  
  val leb : coq_N -> coq_N -> bool
  
  val ltb : coq_N -> coq_N -> bool
  
  val min : coq_N -> coq_N -> coq_N
  
  val max : coq_N -> coq_N -> coq_N
  
  val div2 : coq_N -> coq_N
  
  val even : coq_N -> bool
  
  val odd : coq_N -> bool
  
  val pow : coq_N -> coq_N -> coq_N
  
  val square : coq_N -> coq_N
  
  val log2 : coq_N -> coq_N
  
  val size : coq_N -> coq_N
  
  val size_nat : coq_N -> int
  
  val pos_div_eucl : positive -> coq_N -> coq_N * coq_N
  
  val div_eucl : coq_N -> coq_N -> coq_N * coq_N
  
  val div : coq_N -> coq_N -> coq_N
  
  val modulo : coq_N -> coq_N -> coq_N
  
  val gcd : coq_N -> coq_N -> coq_N
  
  val ggcd : coq_N -> coq_N -> coq_N * (coq_N * coq_N)
  
  val sqrtrem : coq_N -> coq_N * coq_N
  
  val sqrt : coq_N -> coq_N
  
  val coq_lor : coq_N -> coq_N -> coq_N
  
  val coq_land : coq_N -> coq_N -> coq_N
  
  val ldiff : coq_N -> coq_N -> coq_N
  
  val coq_lxor : coq_N -> coq_N -> coq_N
  
  val shiftl_nat : coq_N -> int -> coq_N
  
  val shiftr_nat : coq_N -> int -> coq_N
  
  val shiftl : coq_N -> coq_N -> coq_N
  
  val shiftr : coq_N -> coq_N -> coq_N
  
  val testbit_nat : coq_N -> int -> bool
  
  val testbit : coq_N -> coq_N -> bool
  
  val to_nat : coq_N -> int
  
  val of_nat : int -> coq_N
  
  val iter : coq_N -> ('a1 -> 'a1) -> 'a1 -> 'a1
  
  val eq_dec : coq_N -> coq_N -> bool
  
  val discr : coq_N -> positive option
  
  val binary_rect :
    'a1 -> (coq_N -> 'a1 -> 'a1) -> (coq_N -> 'a1 -> 'a1) -> coq_N -> 'a1
  
  val binary_rec :
    'a1 -> (coq_N -> 'a1 -> 'a1) -> (coq_N -> 'a1 -> 'a1) -> coq_N -> 'a1
  
  val peano_rect : 'a1 -> (coq_N -> 'a1 -> 'a1) -> coq_N -> 'a1
  
  val peano_rec : 'a1 -> (coq_N -> 'a1 -> 'a1) -> coq_N -> 'a1
  
  val leb_spec0 : coq_N -> coq_N -> reflect
  
  val ltb_spec0 : coq_N -> coq_N -> reflect
  
  module Private_BootStrap : 
   sig 
    
   end
  
  val recursion : 'a1 -> (coq_N -> 'a1 -> 'a1) -> coq_N -> 'a1
  
  module Private_OrderTac : 
   sig 
    module Elts : 
     sig 
      type t = coq_N
     end
    
    module Tac : 
     sig 
      
     end
   end
  
  module Private_NZPow : 
   sig 
    
   end
  
  module Private_NZSqrt : 
   sig 
    
   end
  
  val sqrt_up : coq_N -> coq_N
  
  val log2_up : coq_N -> coq_N
  
  module Private_NZDiv : 
   sig 
    
   end
  
  val lcm : coq_N -> coq_N -> coq_N
  
  val eqb_spec : coq_N -> coq_N -> reflect
  
  val b2n : bool -> coq_N
  
  val setbit : coq_N -> coq_N -> coq_N
  
  val clearbit : coq_N -> coq_N -> coq_N
  
  val ones : coq_N -> coq_N
  
  val lnot : coq_N -> coq_N -> coq_N
  
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Rev : 
   sig 
    module ORev : 
     sig 
      type t = coq_N
     end
    
    module MRev : 
     sig 
      val max : coq_N -> coq_N -> coq_N
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
      coq_N -> coq_N -> (coq_N -> coq_N -> __ -> 'a1 -> 'a1) -> (__ -> 'a1)
      -> (__ -> 'a1) -> 'a1
    
    val max_case :
      coq_N -> coq_N -> (coq_N -> coq_N -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 ->
      'a1
    
    val max_dec : coq_N -> coq_N -> bool
    
    val min_case_strong :
      coq_N -> coq_N -> (coq_N -> coq_N -> __ -> 'a1 -> 'a1) -> (__ -> 'a1)
      -> (__ -> 'a1) -> 'a1
    
    val min_case :
      coq_N -> coq_N -> (coq_N -> coq_N -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 ->
      'a1
    
    val min_dec : coq_N -> coq_N -> bool
   end
  
  val max_case_strong : coq_N -> coq_N -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val max_case : coq_N -> coq_N -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : coq_N -> coq_N -> bool
  
  val min_case_strong : coq_N -> coq_N -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val min_case : coq_N -> coq_N -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : coq_N -> coq_N -> bool
 end

val coq_N_rec_double :
  coq_N -> 'a1 -> (coq_N -> 'a1 -> 'a1) -> (coq_N -> 'a1 -> 'a1) -> 'a1

