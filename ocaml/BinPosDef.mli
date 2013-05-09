open BinNums
open Datatypes
open Peano

module Pos : 
 sig 
  type t = positive
  
  val succ : positive -> positive
  
  val add : positive -> positive -> positive
  
  val add_carry : positive -> positive -> positive
  
  val pred_double : positive -> positive
  
  val pred : positive -> positive
  
  val pred_N : positive -> coq_N
  
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
  
  val mask_rect : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1
  
  val mask_rec : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1
  
  val succ_double_mask : mask -> mask
  
  val double_mask : mask -> mask
  
  val double_pred_mask : positive -> mask
  
  val pred_mask : mask -> mask
  
  val sub_mask : positive -> positive -> mask
  
  val sub_mask_carry : positive -> positive -> mask
  
  val sub : positive -> positive -> positive
  
  val mul : positive -> positive -> positive
  
  val iter : positive -> ('a1 -> 'a1) -> 'a1 -> 'a1
  
  val pow : positive -> positive -> positive
  
  val square : positive -> positive
  
  val div2 : positive -> positive
  
  val div2_up : positive -> positive
  
  val size_nat : positive -> int
  
  val size : positive -> positive
  
  val compare_cont : positive -> positive -> int -> int
  
  val compare : positive -> positive -> int
  
  val min : positive -> positive -> positive
  
  val max : positive -> positive -> positive
  
  val eqb : positive -> positive -> bool
  
  val leb : positive -> positive -> bool
  
  val ltb : positive -> positive -> bool
  
  val sqrtrem_step :
    (positive -> positive) -> (positive -> positive) -> (positive * mask) ->
    positive * mask
  
  val sqrtrem : positive -> positive * mask
  
  val sqrt : positive -> positive
  
  val gcdn : int -> positive -> positive -> positive
  
  val gcd : positive -> positive -> positive
  
  val ggcdn : int -> positive -> positive -> positive * (positive * positive)
  
  val ggcd : positive -> positive -> positive * (positive * positive)
  
  val coq_Nsucc_double : coq_N -> coq_N
  
  val coq_Ndouble : coq_N -> coq_N
  
  val coq_lor : positive -> positive -> positive
  
  val coq_land : positive -> positive -> coq_N
  
  val ldiff : positive -> positive -> coq_N
  
  val coq_lxor : positive -> positive -> coq_N
  
  val shiftl_nat : positive -> int -> positive
  
  val shiftr_nat : positive -> int -> positive
  
  val shiftl : positive -> coq_N -> positive
  
  val shiftr : positive -> coq_N -> positive
  
  val testbit_nat : positive -> int -> bool
  
  val testbit : positive -> coq_N -> bool
  
  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1
  
  val to_nat : positive -> int
  
  val of_nat : int -> positive
  
  val of_succ_nat : int -> positive
 end

