open Sexplib.Conv 
open Format

type t =
  | Int64 of Int64.t sexp_opaque 
  | Int48 of Int64.t sexp_opaque
  | Int32 of Int32.t sexp_opaque
  | Int32m of (Int32.t * Int32.t) sexp_opaque (* value, mask *)
  | Int16 of int
  | Int8 of int 
  | Int4 of int 
 with sexp

let get_int (v : t) : int =
  match v with
    | Int64 n | Int48 n -> 
      if n > Int64.of_int max_int then
        raise (Invalid_argument "get_int")
      else 
        Int64.to_int n
    | Int32 n -> 
      if n > Int32.of_int max_int then 
        raise (Invalid_argument "get_int")
      else 
        Int32.to_int n
    | Int32m (n, _) ->
      if n > Int32.of_int max_int then
        raise (Invalid_argument "get_int")
      else
        Int32.to_int n
    | Int16 n -> n
    | Int8 n -> n
    | Int4 n -> n

let get_int64 (v : t) : Int64.t = match v with
  | Int64 n
  | Int48 n -> n
  | Int32 n -> Int64.of_int32 n
  | Int32m (n, _) -> Int64.of_int32 n
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
  | Int32m (n, _) -> Int64.of_int32 n
  | Int16 n
  | Int8 n
  | Int4 n -> Int64.of_int n

let get_int32 (v : t) : Int32.t = match v with
  | Int64 n | Int48 n ->
    if n > 0xFFFFFFFFL then raise (Invalid_argument "get_int32")
    else Int64.to_int32 n
  | Int32 n -> n
  | Int32m (n, _) -> n
  | Int16 n | Int8 n | Int4 n -> Int32.of_int n

(* Returns a (value, mask) pair of 32-bit ints
   Remember: mask = 0 means don't mask any bits.
             this is the opposite of CIDR convention.
 *)
let get_int32m (v : t) : Int32.t * Int32.t = match v with
  | Int64 n | Int48 n ->
    if n > 0xFFFFFFFFL then raise (Invalid_argument "get_int32")
    else (Int64.to_int32 n, 0l)
  | Int32 n -> (n, 0l)
  | Int32m (n, m) -> (n, m)
  | Int16 n | Int8 n | Int4 n -> (Int32.of_int n, 0l)

let int64_to_int (n : Int64.t) : int =
    if n > Int64.of_int max_int then
      raise (Invalid_argument "int64_to_int")
    else
      Int64.to_int n

let int32_to_int (n : Int32.t) : int =
    if n > Int32.of_int max_int then
      raise (Invalid_argument (sprintf "int32_to_int applied to %lx" n))
    else
      Int32.to_int n

let vint_to_int (v : t) : int = match v with
  | Int64 n | Int48 n -> int64_to_int n
  | Int32 n -> int32_to_int n
  | Int32m (n, _) -> int32_to_int n
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
  | Int64 n -> fprintf fmt "%Lu" n
  | Int48 n -> fprintf fmt "%Lu" n
  | Int32 n -> fprintf fmt "%lu" n
  | Int32m (n, m) -> fprintf fmt "%lu/%ld" n m
  | Int16 n -> fprintf fmt "%u" n
  | Int8 n -> fprintf fmt "%u" n
  | Int4 n -> fprintf fmt "%u" n

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

let compare v1 v2 = 
  match v1,v2 with 
  | Int64 n,_ ->
     Int64.compare n (get_int64 v2)
  | _, Int64 n -> 
     Int64.compare (get_int64 v1) n
  | Int48 n,_ ->
     Int64.compare n (get_int48 v2)
  | _, Int48 n -> 
     Int64.compare (get_int48 v1) n
  | Int32m(n1,m1),_ ->
     let n2,m2 = get_int32m v2 in 
     let cmp1 = Int32.compare n1 n2 in 
     if cmp1 <> 0 then cmp1 
     else Int32.compare m1 m2 
  | _,Int32m(n2,m2) ->
     let n1,m1 = get_int32m v1 in 
     let cmp1 = Int32.compare n1 n2 in 
     if cmp1 <> 0 then cmp1 
     else Int32.compare m1 m2 
  | Int32 n,_ ->
     Int32.compare n (get_int32 v2)
  | _, Int32 n -> 
     Int32.compare (get_int32 v1) n
  | Int16 n,_ ->
     Pervasives.compare n (get_int16 v2)
  | _, Int16 n -> 
     Pervasives.compare (get_int16 v1) n
  | Int8 n,_ ->
     Pervasives.compare n (get_int8 v2)
  | _, Int8 n -> 
     Pervasives.compare (get_int8 v1) n
  | Int4 n,_ ->
     Pervasives.compare n (get_int4 v2)

let hash = Hashtbl.hash
