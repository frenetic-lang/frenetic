module Word8 = 
 struct 

  type t = int
  
  (** val zero : t **)
  
  let zero = 0
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end

module Word12 = 
 struct 
  type t = int
  
  (** val zero : t **)
  
  let zero = 0
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end

module Word16 = 
 struct 
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
 end

module Word32 = 
 struct 
  type t = int32
  
  (** val zero : t **)
  
  let zero = Int32.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end

module Word48 = 
 struct 
  type t = int64
  
  (** val zero : t **)
  
  let zero = Int64.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end

module Word64 = 
 struct 
  type t = int64
  
  (** val zero : t **)
  
  let zero = Int64.zero
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end
