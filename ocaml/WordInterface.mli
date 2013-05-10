open BinNums

module type WORD = 
 sig 
  type t 
  
  val width : positive
  
  val eq_dec : t -> t -> bool
  
  val zero : t
 end

module type WIDTH = 
 sig 
  val width : positive
 end

module type MAKEWORD = 
 sig 
  val width : positive
  
  type coq_Word =
    coq_N
    (* singleton inductive, whose constructor was Mk *)
  
  type t = coq_Word
 end

module MakeWord : 
 functor (Width:WIDTH) ->
 sig 
  val width : positive
  
  type coq_Word =
    coq_N
    (* singleton inductive, whose constructor was Mk *)
  
  type t = coq_Word
  
  val zero : t
 end

module Width8 : 
 sig 
  val width : positive
 end

module Width12 : 
 sig 
  val width : positive
 end

module Width16 : 
 sig 
  val width : positive
 end

module Width32 : 
 sig 
  val width : positive
 end

module Width48 : 
 sig 
  val width : positive
 end

module Width64 : 
 sig 
  val width : positive
 end

module Word8 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val beqdec : t -> t -> bool
 end

module Word12 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val beqdec : t -> t -> bool
 end

module Word16 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val to_nat : int -> int
  
  val max_value : t
  
  val pred : int -> int
  
  val beqdec : t -> t -> bool
 end

module Word32 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int32
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val beqdec : t -> t -> bool
 end

module Word48 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int64
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val beqdec : t -> t -> bool
 end

module Word64 : 
 sig 
  module M : 
   sig 
    val width : positive
    
    type coq_Word =
      coq_N
      (* singleton inductive, whose constructor was Mk *)
    
    type t = coq_Word
    
    val zero : t
   end
  
  val width : positive
  
  type t = int64
  
  val zero : t
  
  val eq_dec : t -> t -> bool
  
  val beqdec : t -> t -> bool
 end

