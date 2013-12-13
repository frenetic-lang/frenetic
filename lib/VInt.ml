type t =
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int8 of int
  | Int4 of int

let get_int (v : t) : int =
  try
    match v with
      | Int64 n -> Int64.to_int n
      | Int48 n -> Int64.to_int n
      | Int32 n -> Int32.to_int n
      | Int16 n -> n
      | Int8 n -> n
      | Int4 n -> n
  with _ ->
    raise (Invalid_argument "get_int")

let get_int64 (v : t) : Int64.t = match v with
  | Int64 n
  | Int48 n -> n
  | Int32 n -> Int64.of_int32 n
  | Int16 n
  | Int8 n
  | Int4 n -> Int64.of_int n

let get_int48 (v : t) : Int64.t = match v with
  | Int64 n -> if n > 0xFFFFFFFFFFFFL then 
                    raise (Invalid_argument "get_int48")
               else
                 n
  | Int48 n -> n
  | Int32 n -> Int64.of_int32 n
  | Int16 n
  | Int8 n
  | Int4 n -> Int64.of_int n

let get_int32 (v : t) : Int32.t = match v with
  | Int64 n | Int48 n ->
    if n > 0xFFFFFFFFL then raise (Invalid_argument "get_int32")
    else Int64.to_int32 n
  | Int32 n -> n
  | Int16 n | Int8 n | Int4 n -> Int32.of_int n

let int64_to_int (n : Int64.t) : int =
    if n > Int64.of_int max_int then
      raise (Invalid_argument "int64_to_int")
    else
      Int64.to_int n

let int32_to_int (n : Int32.t) : int =
    if n > Int32.of_int max_int then
      raise (Invalid_argument "int32_to_int")
    else
      Int32.to_int n

let vint_to_int (v : t) : int = match v with
  | Int64 n | Int48 n -> int64_to_int n
  | Int32 n -> int32_to_int n
  | Int16 n | Int8 n | Int4 n -> n

let get_int16 (v : t) : int =
  let n = vint_to_int v in
  if n > 0xFFFF then raise (Invalid_argument "get_int16")
  else n

let get_int8 (v : t) : int =
  let n = vint_to_int v in
  if n > 0xFF then raise (Invalid_argument "get_int8")
  else n

let get_int4 (v : t) : int =
  let n = vint_to_int v in
  if n > 0xF then raise (Invalid_argument "get_int4")
  else n

let format (fmt : Format.formatter) (v : t) : unit = 
	let open Format in
	match v with
  | Int64 n -> fprintf fmt "%Ld" n
  | Int48 n -> fprintf fmt "%Ld" n
  | Int32 n -> fprintf fmt "%ld" n
  | Int16 n -> fprintf fmt "%d" n
  | Int8 n -> fprintf fmt "%d" n
  | Int4 n -> fprintf fmt "%d" n


let get_string v =
  let make_string_of formatter x =
    let open Format in
        let buf = Buffer.create 100 in
        let fmt = formatter_of_buffer buf in
        pp_set_margin fmt 80;
        formatter fmt x;
        fprintf fmt "@?";
        Buffer.contents buf in
  make_string_of format v
