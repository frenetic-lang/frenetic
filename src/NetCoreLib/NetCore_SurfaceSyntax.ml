module Pol = NetCore_Types.Internal
module Env = Map.Make (String)

type pos = Lexing.position

type id = string

type exp =
  | Action of pos * Pol.action
  | Filter of pos * Pol.pred
  | Par of pos * exp * exp
  | Seq of pos * exp * exp
  | ITE of pos * Pol.pred * exp * exp
  | Id of pos * id
  | Let of pos * (id * value) list * exp
  | Transform of pos * (Pol.pol -> Pol.pol) * exp

and value = 
  | Pol of Pol.pol
  | PolStream of Pol.pol NetCore_Stream.t

type env = value Env.t

exception CompileError of string

let sprintf = Format.sprintf

let string_of_pos pos = 
  let open Lexing in
  sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let compile_pol f = function
  | Pol p -> Pol (f p)
  | PolStream p_stream -> PolStream (NetCore_Stream.map f p_stream)

let compile_pol2 f = function
  | (Pol p1, Pol p2) -> 
    Pol (f p1 p2)
  | (PolStream p1_stream, Pol p2) ->
    PolStream
      (NetCore_Stream.map (fun p1 -> f p1 p2) p1_stream)
  | (Pol p1, PolStream p2_stream) ->
    PolStream
      (NetCore_Stream.map (fun p2 -> f p1 p2) p2_stream)
  | (PolStream p1_stream, PolStream p2_stream) ->
      PolStream
      (NetCore_Stream.map2 (fun p1 p2 -> f p1 p2) p1_stream p2_stream)

let rec bind_all pos xs exps env = match (xs, exps) with
  | ([], []) -> env
  | ( x :: xs', exp :: exps') ->
    if List.mem x xs' then
      failwith "duplicate binding"
    else
      bind_all pos xs' exps' (Env.add x exp env)
  | _ -> failwith "destructuring mismatch"

let rec compile (env : env) = function
  | Par (pos, e1, e2) ->
    compile_pol2 
      (fun p1 p2 -> Pol.PoUnion (p1, p2))
      (compile env e1, compile env e2)
  | Seq (pos, e1, e2) ->
    compile_pol2
      (fun p1 p2 -> Pol.PoSeq (p1, p2))
      (compile env e1, compile env e2)
  | Filter (pos, pred) -> Pol (Pol.PoFilter pred)
  | Action (pos, act) -> Pol (Pol.PoAction act)
  | ITE (pos, pred, e1, e2) ->
    compile_pol2
      (fun p1 p2 -> Pol.PoITE (pred, p1, p2))
      (compile env e1, compile env e2)
  | Id (pos, x) ->
    begin 
      try Env.find x env
      with Not_found ->
        raise (CompileError 
                 (sprintf "%s: variable %s is not defined"
                    (string_of_pos pos) x))
    end
  | Let (pos, binds, body) -> 
    let env' = 
      List.fold_left (fun env' (x, e) -> Env.add x e env')
        env binds in
    compile env' body
  | Transform (pos, f, e) -> compile_pol f (compile env e)

let compile_program exp = 
  match compile Env.empty exp with
    | PolStream stream -> stream
    | Pol pol -> NetCore_Stream.constant pol
