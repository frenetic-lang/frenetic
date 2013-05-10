module type MONAD = 
 sig 
  type 'x m 
  
  val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m
  
  val ret : 'a1 -> 'a1 m
 end

module Maybe : 
 sig 
  type 'a m = 'a option
  
  val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option
  
  val ret : 'a1 -> 'a1 option
 end

