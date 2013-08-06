module Pol = NetCore_Types
module Env = Map.Make (String)

type pos = Lexing.position

type id = string

type exp =
  | HandleSwitchEvent of pos * (Pol.switchEvent -> unit)
  | Action of pos * Pol.action
  | Action1 of pos * cexp * (int64 -> Pol.action)
  | Action2 of pos * cexp * cexp * (int64 -> int64 -> Pol.action)
  | Filter of pos * Pol.pred
  | Par of pos * exp * exp
  | Seq of pos * exp * exp
  | ITE of pos * Pol.pred * exp * exp
  | Let of pos * (id * exp) list * exp
  | Transform of pos * (Pol.pol -> Pol.pol) * exp
  | Slice of pos * Pol.pred * exp * Pol.pred
  | CExp of cexp

and cexp =
  | Id of pos * id
  | Value of value

and value = 
  | Const of int64
  | Pol of Pol.pol
  | PolStream of unit Lwt.t * Pol.pol NetCore_Stream.t

type top =
  | Bind of pos * id * exp * top
  | Main of pos * exp
  | Include of pos * string * top
  | Check of pos * string * Pol.pred * exp * Pol.pred * bool option * top

type env = value Env.t

let init_env = 
  Env.add
    "learn"
     (let (init, pol) = NetCore_MacLearning.make () in
      let (lwt_e, stream) = NetCore_Stream.from_stream init pol in
      PolStream (lwt_e, stream))
  Env.empty
