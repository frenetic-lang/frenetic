open Datatypes
open Orders
open OrdersTac

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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

(** val gmax : ('a1 -> 'a1 -> int) -> 'a1 -> 'a1 -> 'a1 **)

let gmax cmp x y =
  match cmp x y with
  | (-1) -> y
  | _ -> x

(** val gmin : ('a1 -> 'a1 -> int) -> 'a1 -> 'a1 -> 'a1 **)

let gmin cmp x y =
  match cmp x y with
  | 1 -> y
  | _ -> x

module GenericMinMax = 
 functor (O:OrderedTypeFull') ->
 struct 
  (** val max : O.t -> O.t -> O.t **)
  
  let max =
    gmax O.compare
  
  (** val min : O.t -> O.t -> O.t **)
  
  let min =
    gmin O.compare
 end

module MaxLogicalProperties = 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 struct 
  module Private_Tac = MakeOrderTac(O)
 end

module MinMaxLogicalProperties = 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Rev = 
   struct 
    module ORev = TotalOrderRev(O)
    
    module MRev = 
     struct 
      (** val max : O.t -> O.t -> O.t **)
      
      let max x y =
        M.min y x
     end
    
    module MPRev = MaxLogicalProperties(ORev)(MRev)
   end
 end

module MinMaxDecProperties = 
 functor (O:OrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  (** val max_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1 **)
  
  let max_case_strong n m compat hl hr =
    let c = coq_CompSpec2Type n m (O.compare n m) in
    (match c with
     | CompGtT -> compat n (M.max n m) __ (hl __)
     | _ -> compat m (M.max n m) __ (hr __))
  
  (** val max_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n m x x0 x1 =
    max_case_strong n m x (fun _ -> x0) (fun _ -> x1)
  
  (** val max_dec : O.t -> O.t -> bool **)
  
  let max_dec n m =
    max_case n m (fun x y _ h0 -> h0) true false
  
  (** val min_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1 **)
  
  let min_case_strong n m compat hl hr =
    let c = coq_CompSpec2Type n m (O.compare n m) in
    (match c with
     | CompGtT -> compat m (M.min n m) __ (hr __)
     | _ -> compat n (M.min n m) __ (hl __))
  
  (** val min_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n m x x0 x1 =
    min_case_strong n m x (fun _ -> x0) (fun _ -> x1)
  
  (** val min_dec : O.t -> O.t -> bool **)
  
  let min_dec n m =
    min_case n m (fun x y _ h0 -> h0) true false
 end

module MinMaxProperties = 
 functor (O:OrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  module OT = OTF_to_TotalOrder(O)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Rev = 
   struct 
    module ORev = 
     struct 
      type t = O.t
     end
    
    module MRev = 
     struct 
      (** val max : O.t -> O.t -> O.t **)
      
      let max x y =
        M.min y x
     end
    
    module MPRev = MaxLogicalProperties(ORev)(MRev)
   end
  
  (** val max_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1 **)
  
  let max_case_strong n m compat hl hr =
    let c = coq_CompSpec2Type n m (O.compare n m) in
    (match c with
     | CompGtT -> compat n (M.max n m) __ (hl __)
     | _ -> compat m (M.max n m) __ (hr __))
  
  (** val max_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n m x x0 x1 =
    max_case_strong n m x (fun _ -> x0) (fun _ -> x1)
  
  (** val max_dec : O.t -> O.t -> bool **)
  
  let max_dec n m =
    max_case n m (fun x y _ h0 -> h0) true false
  
  (** val min_case_strong :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1 **)
  
  let min_case_strong n m compat hl hr =
    let c = coq_CompSpec2Type n m (O.compare n m) in
    (match c with
     | CompGtT -> compat m (M.min n m) __ (hr __)
     | _ -> compat n (M.min n m) __ (hl __))
  
  (** val min_case :
      O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n m x x0 x1 =
    min_case_strong n m x (fun _ -> x0) (fun _ -> x1)
  
  (** val min_dec : O.t -> O.t -> bool **)
  
  let min_dec n m =
    min_case n m (fun x y _ h0 -> h0) true false
 end

module UsualMinMaxLogicalProperties = 
 functor (O:UsualTotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Rev = 
   struct 
    module ORev = 
     struct 
      type t = O.t
     end
    
    module MRev = 
     struct 
      (** val max : O.t -> O.t -> O.t **)
      
      let max x y =
        M.min y x
     end
    
    module MPRev = MaxLogicalProperties(ORev)(MRev)
   end
 end

module UsualMinMaxDecProperties = 
 functor (O:UsualOrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  module Private_Dec = MinMaxDecProperties(O)(M)
  
  (** val max_case_strong :
      O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n m x x0 =
    Private_Dec.max_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n m x x0 =
    max_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : O.t -> O.t -> bool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong :
      O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n m x x0 =
    Private_Dec.min_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n m x x0 =
    min_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : O.t -> O.t -> bool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

module UsualMinMaxProperties = 
 functor (O:UsualOrderedTypeFull') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
  
  val min : O.t -> O.t -> O.t
 end) ->
 struct 
  module OT = OTF_to_TotalOrder(O)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Rev = 
   struct 
    module ORev = 
     struct 
      type t = O.t
     end
    
    module MRev = 
     struct 
      (** val max : O.t -> O.t -> O.t **)
      
      let max x y =
        M.min y x
     end
    
    module MPRev = MaxLogicalProperties(ORev)(MRev)
   end
  
  module Private_Dec = 
   struct 
    (** val max_case_strong :
        O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__
        -> 'a1) -> 'a1 **)
    
    let max_case_strong n m compat hl hr =
      let c = coq_CompSpec2Type n m (O.compare n m) in
      (match c with
       | CompGtT -> compat n (M.max n m) __ (hl __)
       | _ -> compat m (M.max n m) __ (hr __))
    
    (** val max_case :
        O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let max_case n m x x0 x1 =
      max_case_strong n m x (fun _ -> x0) (fun _ -> x1)
    
    (** val max_dec : O.t -> O.t -> bool **)
    
    let max_dec n m =
      max_case n m (fun x y _ h0 -> h0) true false
    
    (** val min_case_strong :
        O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__
        -> 'a1) -> 'a1 **)
    
    let min_case_strong n m compat hl hr =
      let c = coq_CompSpec2Type n m (O.compare n m) in
      (match c with
       | CompGtT -> compat m (M.min n m) __ (hr __)
       | _ -> compat n (M.min n m) __ (hl __))
    
    (** val min_case :
        O.t -> O.t -> (O.t -> O.t -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let min_case n m x x0 x1 =
      min_case_strong n m x (fun _ -> x0) (fun _ -> x1)
    
    (** val min_dec : O.t -> O.t -> bool **)
    
    let min_dec n m =
      min_case n m (fun x y _ h0 -> h0) true false
   end
  
  (** val max_case_strong :
      O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n m x x0 =
    Private_Dec.max_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n m x x0 =
    max_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : O.t -> O.t -> bool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong :
      O.t -> O.t -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n m x x0 =
    Private_Dec.min_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : O.t -> O.t -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n m x x0 =
    min_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : O.t -> O.t -> bool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

module TOMaxEqDec_to_Compare = 
 functor (O:TotalOrder') ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 functor (E:sig 
  val eq_dec : O.t -> O.t -> bool
 end) ->
 struct 
  (** val compare : O.t -> O.t -> int **)
  
  let compare x y =
    if E.eq_dec x y then 0 else if E.eq_dec (M.max x y) y then (-1) else 1
 end

module TOMaxEqDec_to_OTF = 
 functor (O:TotalOrder) ->
 functor (M:sig 
  val max : O.t -> O.t -> O.t
 end) ->
 functor (E:sig 
  val eq_dec : O.t -> O.t -> bool
 end) ->
 struct 
  type t = O.t
  
  (** val eq_dec : O.t -> O.t -> bool **)
  
  let eq_dec =
    E.eq_dec
  
  (** val compare : O.t -> O.t -> int **)
  
  let compare x y =
    if E.eq_dec x y then 0 else if E.eq_dec (M.max x y) y then (-1) else 1
 end

