module K = NetKAT_Types

module HeaderMap = K.HeaderMap

module HeaderValMapSet = Set.Make (struct
    type t = K.header_val HeaderMap.t
    let compare x y = HeaderMap.compare Pervasives.compare x y
  end)

type pred = K.header_val_map

type seq = K.header_val_map

type sum = HeaderValMapSet.t

type local =
  | Action of sum
  | ITE of pred * sum * local

let id : seq = HeaderMap.empty

let drop : sum = HeaderValMapSet.empty

(* Only used by and_pred *)
exception Empty_pred

(* map_sum f (seq_1 + ... + seq_n) = f seq_1 + ... + f seq_n *)
let map_sum (f : seq -> seq) (pol : sum) =
  let g seq pol' = HeaderValMapSet.add (f seq) pol' in
  HeaderValMapSet.fold g pol drop

(* Some (pr1 ; pr2) unless the conjunct in empty, in which case, return None. *)
let rec and_pred (pr1 : pred) (pr2 : pred) : pred option =
  let f hdr val1 val2 = match (val1, val2) with
    | (Some v1, Some v2) -> if v1 <> v2 then raise Empty_pred else Some v1
    | (Some v1, None) -> Some v1
    | (None, Some v2) -> Some v2
    | (None, None) -> failwith "and_pred impossible case" in
  try 
    Some (HeaderMap.merge f pr1 pr2)
  with
    Empty_pred -> None

(* s1 + s2 *)
let rec par_sum_sum (s1 : sum) (s2 : sum) : sum = 
  HeaderValMapSet.union s1 s2

(* Lemma 1: if Y then S + S' else (S + A') = S + (if Y then S' else A')

     if Y then S + S' else (S + A')                      
   = Y;(S + S') + !Y;(S + A')                            if-then-else
   = Y;S + !Y;S + Y;S' + !Y;A'                           distributivity
   = (Y+!Y);S + (Y;S' + !Y;A')                           commutativity
   = 1;S + (Y;S' + !Y;A')                                excluded middle
   = S + (Y;S' + !Y;A')                                  *1
   = S + (if Y then S' else A')                          if-then-else

   QED
*)

(* simpl_par_sum_local S A = S + A *)
let rec simpl_par_sum_local (s : sum) (l : local) : local = match l with
  (* Case A = S' is trivial *)
  | Action s' -> Action (par_sum_sum s s')
  (* Case A = if Y then S' else A'

       if Y then S + S' else [simpl_par_sum_local S A']    RHS below
     = if Y then S + S' else (S + A')                      induction
     = S + (if Y then S' else A')                          Lemma 1
     = S + A                                               substitution
  *)
  | ITE (y, s', a') -> ITE (y, par_sum_sum s s', simpl_par_sum_local s a')

(* par_sum_local X S A B = if X then S + A else B *)
let rec par_sum_local (x : pred) (s : sum) (a : local) (b : local) : local = 
  match a with
  (* Case A = S'

       if X then S + S' else B                                        RHS below
     = if X then S + A else B                                      substitution
  *)
  | Action s' -> ITE(x, par_sum_sum s s', b)
  (* Case A = if Y then S' else A'

       if X; Y then S + S' else [par_sum_local X S A' B]              RHS below
     = if X; Y then S + S' else if X then S + A' else B               induction
     = X;Y;(S + S') + !(X;Y);(if X then S + A' else B)             if-then-else
     = X;Y;(S + S') + (!X+!Y);(if X then S + A' else B)          de Morgans law
     = X;Y;(S + S') + !X;(if X then S + A' else B)               distributivity
                    + !Y;(if X then S + A' else B)         
     = X;Y;(S + S') + !X;B + !Y;(if X then S + A' else B)         contradiction
     = X;Y;(S + S') + !X;B + !Y;(X;(S+A') + !X;B)                  if-then-else
     = X;Y;(S + S') + !X;B + !Y;X;(S+A') + !Y;!X;B               distributivity
     = X;Y;(S + S') + !Y;X;(S+A') + (1+!Y);!X;B                  distributivity
     = X;Y;(S + S') + !Y;X;(S+A') + !X;B                                    b+1             
     = if X then Y;(S + S') + !Y;(S + A') else B                   if-then-else
     = if X then (if Y then S + S' else S + A') else B             if-then-else
     = if X then (S + if Y then S' else A') else B                      Lemma 1
     = if X then (S + A) else B                                    substitution
  *)
  | ITE (y, s', a') -> 
    let else_branch = par_sum_local x s a' b in
    match and_pred x y with
    | None -> else_branch
    | Some x_and_y ->
      ITE (x_and_y, par_sum_sum s s', else_branch)

(* par_local_local A B = A + B *)
let rec par_local_local (a : local) (b : local) : local = match (a, b) with
  (* Immediate *)
  | (_ , Action s) -> simpl_par_sum_local s a
  (* Commutativity, then immediate *)
  | (Action s, _) -> simpl_par_sum_local s b
  (* Case A = if X then A' else B'

       par_sum_local X A' B (par_local_local B' B)                    RHS below
     = par_sum_local X A' B (B' + B)                                  induction
     = if X then A' + B else B' + B                               par_sum_local
     = (if X then A' else B') + B                                       Lemma 1
     = A + B                                                       substitution
  *)
  | (ITE (x, a', b'), _) ->  par_sum_local x a' b (par_local_local b' b)

(* seq_seq H V SEQ = H<-V; SEQ *)
let seq_seq (h : K.header) (v : K.header_val) (seq : seq) : seq = 
  if HeaderMap.mem h seq then
    seq
  else
    HeaderMap.add h v seq

(* commute_sum H V S = H <- V; S *)
let commute_sum (h : K.header) (v : K.header_val) (s : sum) : sum =
  map_sum (seq_seq h v) s (* distributivity *)

(* pr is a conjunct, so if h is not bound, then it is matches h = v *)
let rec pred_matches (h : K.header) (v : K.header_val) (pr : pred) : bool =
  not (HeaderMap.mem h pr) || HeaderMap.find h pr = v

let rec pred_wildcard (h : K.header) (pr : pred) : pred =
  HeaderMap.remove h pr

(* commute H V P = H<-V; P *)
let rec commute (h : K.header) (v : K.header_val) (p : local) : local = match p with
  | Action sum -> Action (commute_sum h v sum)
  (* Case P = if X then S else Q

       H <- V; if X then S else Q
     = H <- V; (H=V + !H=V); if X then S else Q
     = H<-V;H=V;if X then S else Q + H<-V;!H=V;if X then S else Q 
     = CONTINUE
  *)
  | ITE (pr, sum, pol) ->
    match pred_matches h v pr with
    | false -> commute h v pol
    | true -> ITE (pred_wildcard h pr, commute_sum h v sum, commute h v pol)

(* returns a term equivalent to [if pr then pol1 else pol2] *)
let rec norm_ite (pr : pred) (pol1 : local) (pol2 : local) : local =
  match pol1 with
  | Action sum1 -> ITE (pr, sum1, pol2)
  | ITE (pr1', sum1', pol1') ->
    match and_pred pr pr1' with
    | None -> norm_ite pr pol1' pol2
    | Some pr1_and_pr1' -> ITE (pr1_and_pr1', sum1', norm_ite pr pol1' pol2)

let seq_seq_local (p1 : seq) (p2 : local) : local = 
  HeaderMap.fold commute  p1 p2

(* p1; p2 *)
let seq_sum_local (p1 : sum) (p2 : local) : local = 
  let lst = HeaderValMapSet.fold (fun elt lst -> elt :: lst) p1 [] in
  let rec loop lst = match lst with
    | [] -> Action drop
    | seq :: lst' ->
      par_local_local (seq_seq_local seq p2) (loop lst') in
  loop lst

let rec seq_local_local (pol1 : local) (pol2 : local) : local = match pol1 with
  | Action sum1 -> seq_sum_local sum1 pol2
  | ITE (pred1, sum1, pol1') ->
    norm_ite pred1 (seq_sum_local sum1 pol2) (seq_local_local pol1' pol2)


let is_drop (pol : sum) : bool =
  HeaderValMapSet.is_empty pol

(* there is only one element, and it is the empty sequence of updates *)
let is_id (pol : sum) : bool =
  not (HeaderValMapSet.is_empty pol) && HeaderMap.is_empty (HeaderValMapSet.choose pol)

(*
    !(if A then X else IND)
  = !(A; X + !A; IND)           by definition of if .. then .. else
  = !(A; X) ; !(!A; IND)        de Morgan's law

  Case X = 1:

      !(A; X) ; !(!A; IND)       from above
    = !(A; 1) ; !(!A; IND)       substitute X = 1
    = !A; !(!A; IND)             identity
    = !A; (!!A + !IND)           de Morgan's law
    = !A; (A + !IND)             double negation
    = !A; A + !A; !IND           distributivity
    = 0 + !A; !IND               excluded middle
    = if A then 0 else 1; !IND   by defn. of if-then-else

    !IND can be normalized by induction and the product can be normalized
    by the seq_local_local function.

  Case X = 0:

      !(A; X) ; !(!A; IND)       from above
    = !(A; 0) ; !(!A; IND)       substitute X = 0
    = !0      ; !(!A; IND)       zero
    = 1; !(!A; IND)              negation
    = !(!A; IND)                 identity
    = !!A + !IND                 de Morgan's law
    = A + !IND                   double negation
    = if A then 1 else 0 + !IND  by defn. of if-then-else

    !IND can be normalized by induction and the sum can be normalized
    by the par_local_local function.
*)
let rec negate (pred : local) : local = match pred with
  | Action sum ->
    if is_drop sum then
      Action (HeaderValMapSet.singleton id)
    else if is_id sum then
      Action drop
    else
      failwith "not a predicate"
  | ITE (a, x, ind) ->
    if is_drop x then
      par_local_local 
        (ITE (a, HeaderValMapSet.singleton id, Action drop))
        (negate ind)
    else if is_id x then
      seq_local_local
        (ITE (a, drop, Action (HeaderValMapSet.singleton id)))
        (negate ind)
    else
      failwith "not a predicate"

let rec pred_local (pr : K.pred) : local = match pr with
  | K.Drop ->
    Action drop (* missing from appendix *)
  | K.Id ->
    Action (HeaderValMapSet.singleton HeaderMap.empty) (* missing from appendix *)
  | K.Neg p ->
    negate (pred_local p)
  | K.Test (h, v) ->
    ITE (HeaderMap.singleton h v, HeaderValMapSet.singleton id, Action drop)
  | K.Or (pr1, pr2) ->
    par_local_local (pred_local pr1) (pred_local pr2)
  | K.And (pr1, pr2) ->
    seq_local_local (pred_local pr1) (pred_local pr2)

let rec star_local (a:local) : local = match a with 
  | Action s -> 
    a (* TODO: stub *)
  | ITE(p, s, b) -> 
    a (* TODO: stub *)

and local_normalize (pol : K.policy) : local = match pol with
  | K.Filter pr -> pred_local pr
  | K.Mod (K.Switch, _) ->
    failwith "unexpected Switch in local_normalize"
  | K.Mod (h, v) ->
    Action (HeaderValMapSet.singleton (HeaderMap.singleton h v))
  | K.Par (pol1, pol2) ->
    (* In Lemma 30, cases 2--4 p,q should be written using if..then..else
       shorthand. *)
    par_local_local (local_normalize pol1) (local_normalize pol2)
  | K.Seq (pol1, pol2) ->
    seq_local_local (local_normalize pol1) (local_normalize pol2)
  | K.Star pol -> 
    star_local (local_normalize pol)
      
let compile = local_normalize

let pred_to_netkat pr =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if HeaderMap.is_empty pr then
    K.Id
  else
    let (h, v) = HeaderMap.min_binding pr in
    let pr' = HeaderMap.remove h pr in
    let f h v pol = K.And (K.Test (h, v), pol) in
    (HeaderMap.fold f pr' (K.Test (h, v)))

let seq_to_netkat (pol : seq) : K.policy =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if HeaderMap.is_empty pol then
    K.Filter K.Id
  else
    let (h, v) = HeaderMap.min_binding pol in
    let pol' = HeaderMap.remove h pol in
    let f h v pol' = K.Seq (K.Mod (h, v), pol') in
    HeaderMap.fold f pol' (K.Mod  (h, v))

let sum_to_netkat (pol : sum) : K.policy = 
  (* avoid printing trailing K.Drop if it is unnecessary *)
  if HeaderValMapSet.is_empty pol then
    K.Filter K.Drop
  else
    let f seq pol' = K.Par (seq_to_netkat seq, pol') in
    let seq = HeaderValMapSet.min_elt pol in
    let pol' = HeaderValMapSet.remove seq pol in
    HeaderValMapSet.fold f pol' (seq_to_netkat seq)

let rec to_netkat (pol : local) : K.policy = match pol with
  | Action sum -> sum_to_netkat sum
  | ITE (pred, sum, pol') ->
    let pr = pred_to_netkat pred in
    K.Par (K.Seq (K.Filter pr, sum_to_netkat sum),
           K.Seq (K.Filter (K.Neg pr), to_netkat pol'))
