module Word8 = 
 struct 
  type t = int
  let zero = 0
  let eq_dec = (=)
  let to_string = string_of_int
 end

module Word12 = 
 struct 
  type t = int
  let zero = 0
  let eq_dec = (=)
  let to_string = string_of_int
 end

module Word16 = 
 struct 
  type t = int
  let zero = 0
  let eq_dec = (=)
  let to_nat = (fun x -> x)
  let max_value = 65535
  let pred = (fun n -> if n = 0 then 0 else n - 1)
  let to_string = string_of_int
 end

module Word32 = 
 struct 
  type t = int32
  let zero = Int32.zero
  let eq_dec = (=)
  let to_string = Int32.to_string
 end

module Word48 = 
 struct 
  type t = int64
  let zero = Int64.zero
  let eq_dec = (=)
  let to_string = Int64.to_string
 end

module Word64 = 
 struct 
  type t = int64
  let zero = Int64.zero
  let eq_dec = (=)
  let to_string = Int64.to_string
 end
