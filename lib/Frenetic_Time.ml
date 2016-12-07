external frenetic_gettime : unit -> Int64.t = "frenetic_gettime"

type timing = (string * Int64.t)

let string_of_timing t =
  Printf.sprintf "%s: %Ld" (fst t) (snd t)

let time () = frenetic_gettime ()

let from (start:Int64.t) =
  Int64.sub (time ()) start

let to_secs nsecs = Int64.div nsecs 1000000000L

let to_msecs nsecs = Int64.div nsecs 1000000L

let to_fsecs nsecs = (Int64.to_float nsecs) /. (Int64.to_float 1000000000L)

let columnize ?(prefix="|") timings =
  let open Core.Std in
  let header = String.concat ~sep:"\t" ( prefix::(List.map ~f:fst timings) ) in
  let data = String.concat ~sep:"\t" (prefix::(List.map ~f:(fun t ->
      sprintf "%Ld" (to_msecs (snd t)))
      timings)) in
  header ^ "\n" ^ data
