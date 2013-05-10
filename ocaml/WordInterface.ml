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

module MakeWord = 
 functor (Width:WIDTH) ->
 struct 
  (** val width : positive **)
  
  let width =
    Width.width
  
  type coq_Word =
    coq_N
    (* singleton inductive, whose constructor was Mk *)
  
  type t = coq_Word
  
  (** val zero : t **)
  
  let zero =
    N0
 end

module Width8 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xO Coq_xH))
 end

module Width12 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xI Coq_xH))
 end

module Width16 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
 end

module Width32 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
 end

module Width48 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
 end

module Width64 = 
 struct 
  (** val width : positive **)
  
  let width =
    Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
 end

module Word8 = 
 struct 
  module M = MakeWord(Width8)
  
  (** val width : positive **)
  
  let width =
    Width8.width
  
  type t = int
  
  (** val zero : t **)
  
  let zero = 0
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

module Word12 = 
 struct 
  module M = MakeWord(Width12)
  
  (** val width : positive **)
  
  let width =
    Width12.width
  
  type t = int
  
  (** val zero : t **)
  
  let zero = 0
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

module Word16 = 
 struct 
  module M = MakeWord(Width16)
  
  (** val width : positive **)
  
  let width =
    Width16.width
  
  type t = int
  
  (** val zero : t **)
  
  let zero = 0
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val to_nat : int -> int **)
  
  let to_nat = (fun x -> x)
  
  (** val max_value : t **)
  
  let max_value = 65535
  
  (** val pred : int -> int **)
  
  let pred = (fun n -> if n = 0 then 0 else n - 1)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

module Word32 = 
 struct 
  module M = MakeWord(Width32)
  
  (** val width : positive **)
  
  let width =
    Width32.width
  
  type t = int32
  
  (** val zero : t **)
  
  let zero = Int32.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

module Word48 = 
 struct 
  module M = MakeWord(Width48)
  
  (** val width : positive **)
  
  let width =
    Width48.width
  
  type t = int64
  
  (** val zero : t **)
  
  let zero = Int64.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

module Word64 = 
 struct 
  module M = MakeWord(Width64)
  
  (** val width : positive **)
  
  let width =
    Width64.width
  
  type t = int64
  
  (** val zero : t **)
  
  let zero = Int64.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
  
  (** val beqdec : t -> t -> bool **)
  
  let beqdec m n =
    if eq_dec m n then true else false
 end

