type t =
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int8 of int
  | Int4 of int

let get_int64 (v : t) : Int64.t = match v with
  | Int64 n -> n
  | _ -> raise (Invalid_argument "get_int64")

let get_int48 (v : t) : Int64.t = match v with
  | Int48 n -> n
  | _ -> raise (Invalid_argument "get_int48")

let get_int32 (v : t) : Int32.t = match v with
  | Int32 n -> n
  | _ -> raise (Invalid_argument "get_int32")

let get_int16 (v : t) : int = match v with
  | Int16 n -> n
  | _ -> raise (Invalid_argument "get_int16")

let get_int8 (v : t) : int = match v with
  | Int8 n -> n
  | _ -> raise (Invalid_argument "get_int8")

let get_int4 (v : t) : int = match v with
  | Int4 n -> n
  | _ -> raise (Invalid_argument "get_int4")

let format (fmt : Format.formatter) (v : t) : unit = 
	let open Format in
	match v with
  | Int64 n -> fprintf fmt "%Ld" n
  | Int48 n -> fprintf fmt "%Ld" n
  | Int32 n -> fprintf fmt "%ld" n
  | Int16 n -> fprintf fmt "%d" n
  | Int8 n -> fprintf fmt "%d" n
  | Int4 n -> fprintf fmt "%d" n
