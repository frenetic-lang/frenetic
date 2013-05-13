module type MONAD = 
 sig 
  type 'x m 
  
  val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m
  
  val ret : 'a1 -> 'a1 m
 end

module Maybe = 
 struct 
  type 'a m = 'a option
  
  (** val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option **)
  
  let bind m0 f =
    match m0 with
    | Some a -> f a
    | None -> None
  
  (** val ret : 'a1 -> 'a1 option **)
  
  let ret x =
    Some x
 end

