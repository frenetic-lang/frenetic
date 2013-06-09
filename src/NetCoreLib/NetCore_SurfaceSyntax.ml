module Pol = NetCore_Types
module Env = Map.Make (String)

type pos = Lexing.position

type id = string

type exp =
  | HandleSwitchEvent of pos * (Pol.switchEvent -> unit)
  | Action of pos * Pol.action
  | Filter of pos * Pol.pred
  | Par of pos * exp * exp
  | Seq of pos * exp * exp
  | ITE of pos * Pol.pred * exp * exp
  | Id of pos * id
  | Let of pos * (id * exp) list * exp
  | Transform of pos * (Pol.pol -> Pol.pol) * exp
  | Slice of pos * Pol.pred * exp * Pol.pred
  | Value of value

and value = 
  | Pol of Pol.pol
  | PolStream of unit Lwt.t * Pol.pol NetCore_Stream.t

type env = value Env.t

let init_env = 
  Env.add
    "learn"
     (let (init, pol) = NetCore_MacLearning.make () in
      let (lwt_e, stream) = NetCore_Stream.from_stream init pol in
      PolStream (lwt_e, stream))
  Env.empty
