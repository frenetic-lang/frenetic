open OpenFlow0x04Types

type event =
| SwitchConnected of switchId
| SwitchDisconnected of switchId
| SwitchMessage of switchId * xid * message

val event_rect :
  (switchId -> 'a1) -> (switchId -> 'a1) -> (switchId -> xid -> message ->
  'a1) -> event -> 'a1

val event_rec :
  (switchId -> 'a1) -> (switchId -> 'a1) -> (switchId -> xid -> message ->
  'a1) -> event -> 'a1

module type CONTROLLER_MONAD = 
 sig 
  type 'x m 
  
  val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m
  
  val ret : 'a1 -> 'a1 m
  
  type state 
  
  val get : state m
  
  val put : state -> unit m
  
  val send : switchId -> xid -> message -> unit m
  
  val recv : event m
  
  val forever : unit m -> unit m
 end

