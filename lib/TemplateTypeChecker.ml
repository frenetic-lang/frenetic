exception Type_error of string

module TS = TemplateSyntax


let rec is_subtype (s : TS.typ) (t : TS.typ) : bool = 
  let open TS in
  match (s, t) with
  | TPred, TPred -> true
  | THdr m, THdr n -> m = n
  | TInt m, TInt n -> m <= n
  | TPol, TPol -> true
  | TPred, TPol -> true
  | TFun (args1, r1), TFun (args2, r2) ->
    List.length args1 = List.length args2 &&
    is_subtype r1 r2 &&
    List.for_all2 is_subtype args2 args1
  | _ -> false

module Env = Map.Make (String)

type env = TS.typ Env.t

let env_add    = Env.add
let env_lookup = Env.find
let empty_env  = Env.empty


let sprintf = Format.sprintf

let string_of_pos pos = 
  let open Lexing in
  sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)



module NKT = Types


let rec get_msb_loc (x : Int64.t) : int =
  if x <= Int64.zero then 0
  else 1 + get_msb_loc (Int64.shift_right_logical x 1)

let rec synth (env : env) (e : TS.exp) : TS.typ =

  match e with 

  | TS.Id   (p, x) -> 
    (try 
       env_lookup x env 
     with Not_found -> 
       raise 
         (Type_error
            (sprintf "%s: Unbound identifier %s" (string_of_pos p) x)))

  | TS.Let  (_, x, with_e, in_e) -> 
    let with_e_t = synth env with_e in
    let env' = env_add x with_e_t env in
    synth env' in_e

  | TS.Fun  (p, param_list, body) -> 
    raise 
      (Type_error 
         (sprintf "%s: Cannot synthesize type of function" (string_of_pos p)))

  | TS.App  (p, f, arg_list) -> 
    (* 
         * synthesize the type of f and see that it is a function
         * "check" that type of each element in arg_list matches corresponding
         * function param type
         * if it does then synth should return type of function body
         * else it should throw a type error
         *)

    (match synth env f with
     | TS.TFun (t_param_list, t_body) -> 
       (try
          let open List in
          let f (e' : TS.exp) 
              (t' : TS.typ) 
              (acc : bool) = (check env e' t') && acc in
          let res = fold_right2 f arg_list t_param_list true in
          if res 
          then t_body
          else raise 
              (Type_error 
                 (sprintf "%s: Wrong type of argument applied to function" (string_of_pos p)))
        with Invalid_argument _ -> raise 
                                     (Type_error 
                                        (sprintf "%s: Mismatched number of arguments/parameters to function" (string_of_pos p))))


     | _ -> raise
              (Type_error
                 (sprintf "%s: Expected a function" (string_of_pos p))))


  | TS.If  (p, e_cond, e_true, e_false) ->
    if not (check env e_cond TS.TPred)
    then raise
        (Type_error
           (sprintf "%s: Condition should be predicate in \"if\"" (string_of_pos p)))
    else 
    if not (check env e_true TS.TPol)
    then raise
        (Type_error
           (sprintf "%s: True branch should be a policy in \"if\"" (string_of_pos p)))
    else if not (check env e_false TS.TPol)
    then raise
        (Type_error
           (sprintf "%s: False branch should be a policy in \"if\"" (string_of_pos p)))
    else TS.TPol

  | TS.Par (p, e1, e2)
  | TS.Seq (p, e1, e2) ->
    if check env e1 TS.TPol &&
       check env e2 TS.TPol
    then TS.TPol
    else raise
        (Type_error
           (sprintf "%s: Type error in \"+\" or \";\"" (string_of_pos p)))

  | TS.Filter  (p, e) ->
    if check env e TS.TPred
    then TS.TPred
    else raise
        (Type_error
           (sprintf "%s: Expected a predicate with \"filter\"" (string_of_pos p)))

  | TS.Link (p, _, _, _, _) ->
    if check env e TS.TPol
    then TS.TPol
    else raise
        (Type_error
           (sprintf "%s: Mismatched types" (string_of_pos p)))

  | TS.True  (_)
  | TS.False  (_) ->
    TS.TPred

  | TS.Mod  (p, e1, e2) ->
    let t_e1 = synth env e1 in
    let t_e2 = synth env e2 in
    (match t_e1, t_e2 with
     | TS.THdr w1, TS.TInt w2 ->
       if w1 >= w2
       then TS.TPol
       else raise
           (Type_error
              (sprintf "%s: Value is greater than what header can accomodate" (string_of_pos p)))

     | _ -> raise
              (Type_error
                 (sprintf "%s: Mismatched type to \":=\"" (string_of_pos p))))

  | TS.Test  (p, e1, e2) ->
    let t_e1 = synth env e1 in
    let t_e2 = synth env e2 in
    (match t_e1, t_e2 with
     | TS.THdr w1, TS.TInt w2 ->
       if w1 >= w2
       then TS.TPred
       else raise
           (Type_error
              (sprintf "%s: Value is greater than header capacity" (string_of_pos p)))

     | _ -> raise
              (Type_error
                 (sprintf "%s: Mismatched type to \"=\"" (string_of_pos p))))


  | TS.And  (p, e1, e2)
  | TS.Or  (p, e1, e2) ->
    if check env e1 TS.TPred &&
       check env e2 TS.TPred
    then TS.TPred
    else raise 
        (Type_error
           (sprintf "%s: \"&&\" and \"||\"  expects to work on predicates" (string_of_pos p)))

  | TS.Neg  (p, e) ->
    if check env e TS.TPred
    then TS.TPred
    else raise
        (Type_error
           (sprintf "%s: \"!\" expects to work on predicates" (string_of_pos p)))

  | TS.Header  (_, hdr) ->
    let module SDNH = Types in
    (match hdr with
     | SDNH.Header (h) -> 
       let module SDNT = SDN_Types in
       (match h with
        | SDNT.IPProto -> TS.THdr (TS.ipproto_width)
        | SDNT.EthType -> TS.THdr (TS.ethtyp_width)

        | SDNT.EthSrc
        | SDNT.EthDst -> TS.THdr (TS.macaddr_width)

        | SDNT.Vlan
        | SDNT.VlanPcp -> TS.THdr (TS.vlan_width)

        | SDNT.IP4Src
        | SDNT.IP4Dst -> TS.THdr (TS.ipaddr_width)

        | SDNT.InPort
        | SDNT.TCPSrcPort
        | SDNT.TCPDstPort -> TS.THdr (TS.port_width))

     | SDNH.Switch  -> TS.THdr (64))


  | TS.HeaderVal  (_, hdr_val) ->
    let msb_loc = get_msb_loc (VInt.get_int64 hdr_val)
    in TS.TInt (msb_loc)


  | TS.TypeIs  (p, e, t) ->
    if check env e t
    then t
    else raise
        (Type_error
           (sprintf "%s: Incorrect type annotation" (string_of_pos p)))

and check (env : env) (e : TS.exp) (t : TS.typ) : bool =

  match e with

  | TS.Id (p, x) -> 
    (try
       let t' = env_lookup x env in
       is_subtype  t' t
     with Not_found -> raise
                         (Type_error
                            (sprintf "%s: Unbound Identifier %s" (string_of_pos p) x)))

  | TS.Let (_, x, with_e, in_e) ->
    let env' = env_add x (synth env with_e) env
    in check env' in_e t

  | TS.Fun (p, params, body) -> 
    (match t with
     | TS.TFun (t_params, t_body) ->
             (*
              * 1. Augment environment with new types involved in t_param_list
              * 2. check if type of body matches with 't' *)

       (try
          let open List in 
          let env' = fold_right2 env_add params t_params env in
          check env' body t_body
        with Invalid_argument _ -> raise
                                     (Type_error
                                        (sprintf "%s: Mismatched number of arguments to function application" (string_of_pos p))))

     | _ -> false)

  | TS.App (_, f, args) ->
    let open List in
    let t_args = map (synth env) args in
    check env f (TS.TFun (t_args, t))


  | TS.If (_, e_cond, e_true, e_false) ->
    (check env e_cond  TS.TPred) &&
    (check env e_true  TS.TPol)  &&
    (check env e_false TS.TPol)  &&
    (t = TS.TPol)

  | TS.Par (_, e1, e2)
  | TS.Seq (_, e1, e2) ->
    check env e1 TS.TPol &&
    check env e2 TS.TPol &&
    t = TS.TPol


  | TS.Filter (_, e) ->
    (check env e TS.TPred) && (is_subtype TS.TPred t)

  | TS.Link (p, sw_e, pt_e, sw_e', pt_e') ->
    check env sw_e  (TS.TInt (TS.switch_width)) &&
    check env pt_e  (TS.TInt (TS.port_width))   &&
    check env sw_e' (TS.TInt (TS.switch_width)) &&
    check env pt_e' (TS.TInt (TS.port_width))

  | TS.True (_)
  | TS.False (_) -> is_subtype TS.TPred t

  | TS.Mod (_, e1, e2) ->
    (TS.TPol = synth env e) && (t = TS.TPol)

  | TS.Test (_, e1, e2) ->
    (TS.TPred = synth env e) && (is_subtype TS.TPred t)

  | TS.And (_, e1, e2)
  | TS.Or  (_, e1, e2) ->
    check env e1 TS.TPred &&
    check env e2 TS.TPred &&
    is_subtype TS.TPred t

  | TS.Neg (_, e) ->
    check env e TS.TPred && is_subtype TS.TPred t

  | TS.Header (_, h) ->
    (match synth env e, t with
     | TS.THdr w1, TS.THdr w2 -> w1 = w2
     | _ -> false)

  | TS.HeaderVal (_, hv) -> is_subtype (synth env e) t

  | TS.TypeIs (_, e', t') ->
    (check env e' t') && (is_subtype t' t)


let type_check (e : TS.exp) (t : TS.typ) : bool =
  check empty_env e t
