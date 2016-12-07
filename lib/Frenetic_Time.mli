
type timing = (string * Int64.t)
val string_of_timing : timing -> string

val time : unit -> Int64.t
val from : Int64.t -> Int64.t
val to_secs : Int64.t -> Int64.t
val to_msecs : Int64.t -> Int64.t
val to_fsecs : Int64.t -> float
val columnize : ?prefix:string -> timing list -> string
