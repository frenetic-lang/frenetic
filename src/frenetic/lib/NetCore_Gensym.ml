type t = string ref

let next_int = ref 0

let gensym () =
  let n = !next_int in
  incr next_int;
  ref ("gensym" ^ string_of_int n)

let gensym_printing str =
  ref str

let to_string sym = !sym
