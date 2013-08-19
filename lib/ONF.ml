module K = NetKAT_Types

module HdrMap = Map.Make (struct
  type t = K.hdr
  let compare = Pervasives.compare
end)

module HdrValSet = Set.Make (struct
  type t = K.hdrVal HdrMap.t
  let compare x y = HdrMap.compare Pervasives.compare x y
end)

type pred = K.hdrVal HdrMap.t

type seq = K.hdrVal HdrMap.t

type sum = HdrValSet.t

type local =
  | Action of sum
  | ITE of pred * sum * local

let id : seq = HdrMap.empty

let drop : sum = HdrValSet.empty

exception Empty_pred

let rec and_pred (pr1 : pred) (pr2 : pred) : pred option =
	let f hdr val1 val2 = match (val1, val2) with
	| (Some v1, Some v2) -> if v1 <> v2 then raise Empty_pred else Some v1
	| (Some v1, None) -> Some v1
	| (None, Some v2) -> Some v2
	| (None, None) -> failwith "and_pred impossible case" in
	try 
    Some (HdrMap.merge f pr1 pr2)
  with
    Empty_pred -> None

let rec par_sum_sum (s1 : sum) (s2 : sum) : sum = 
  HdrValSet.union s1 s2

let rec par_sum_local (s : sum) (l : local) : local = match l with
  | Action s' -> Action (par_sum_sum s s')
  | ITE (pr, s', l') -> ITE (pr, par_sum_sum s s', par_sum_local s l')

let rec par_local_local (p1 : local) (p2 : local) : local = match (p1, p2) with
  | (_ , Action s2) -> par_sum_local s2 p1
  | (Action s1, _) -> par_sum_local s1 p2
  | (ITE (pr1, s1, p1'), ITE (pr2, s2, p2')) ->
    match and_pred pr1 pr2 with
    | None -> 
      ITE (pr1, s1,
      	ITE (pr2, s2, 
      	  Action drop))
    | Some pr1_and_pr2 ->
	    ITE (pr1_and_pr2, par_sum_sum s1 s2,
	      ITE (pr1, s1,
	      	ITE (pr2, s2,
	      	  Action drop)))

(* equivalent to h<-v; seq *)
let seq_seq (h : K.hdr) (v : K.hdrVal) (seq : seq) : seq = 
	if HdrMap.mem h seq then
	  seq
	else
	  HdrMap.add h v seq

(* returns a term equivalent to h<-v ; s *)
let rec commute_sum h v s =
  let f seq sum = HdrValSet.add (seq_seq h v seq) sum in
  HdrValSet.fold f s drop

(* pr is a conjunct, so if h is not bound, then it is matches h = v *)
let rec pred_matches (h : K.hdr) (v : K.hdrVal) (pr : pred) : bool =
  not (HdrMap.mem h pr) || HdrMap.find h pr = v

let rec pred_wildcard (h : K.hdr) (pr : pred) : pred =
	  HdrMap.remove h pr

(* returns a term equivalent to h<-v; p *)
let rec commute (h : K.hdr) (v : K.hdrVal) (p : local) : local = match p with
  | Action sum -> Action (commute_sum h v sum)
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
	HdrMap.fold commute  p1 p2

(* p1; p2 *)
let seq_sum_local (p1 : sum) (p2 : local) : local = 
  let lst = HdrValSet.fold (fun elt lst -> elt :: lst) p1 [] in
  let rec loop lst = match lst with
    | [] -> Action drop
    | seq :: lst' ->
      par_local_local (seq_seq_local seq p2) (loop lst') in
  loop lst

let rec seq_local_local (pol1 : local) (pol2 : local) : local = match pol1 with
  | Action sum1 -> seq_sum_local sum1 pol2
  | ITE (pred1, sum1, pol1') ->
    norm_ite pred1 (seq_sum_local sum1 pol2) (seq_local_local pol1' pol2)

let rec negate_sum (pol : sum) : sum = match HdrValSet.cardinal pol with
  | 0 -> HdrValSet.singleton id
  | 1 -> if HdrMap.is_empty (HdrValSet.choose pol) then
          drop
        else
          failwith "negating a non-policy (or not normalized)"
  | _ -> failwith "negating a non-policy (or not normalized)"

let rec negate (pol : local) : local = match pol with
  | Action sum -> Action (negate_sum sum)
  | ITE (pr, sum, pol') -> ITE (pr, negate_sum sum, negate pol')

let rec local_normalize (pol : K.pol) : local = match pol with
  | K.Drop ->
    Action drop (* missing from appendix *)
  | K.Id ->
    Action (HdrValSet.singleton HdrMap.empty) (* missing from appendix *)
  | K.Neg p ->
    negate (local_normalize p)
  | K.Test (h, v) ->
    ITE (HdrMap.singleton h v, HdrValSet.singleton id, Action drop)
  | K.Set (K.Switch, _) ->
    failwith "unexpected Switch in local_normalize"
  | K.Set (h, v) ->
    Action (HdrValSet.singleton (HdrMap.singleton h v))
  | K.Par (pol1, pol2) ->
    (* In Lemma 30, cases 2--4 p,q should be written using if..then..else
       shorthand. *)
    par_local_local (local_normalize pol1) (local_normalize pol2)
  | K.Seq (pol1, pol2) ->
    seq_local_local (local_normalize pol1) (local_normalize pol2)

let compile = local_normalize

let pred_to_netkat pr =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if HdrMap.is_empty pr then
    K.Id
  else
    let (h, v) = HdrMap.min_binding pr in
    let pr' = HdrMap.remove h pr in
	  let f h v pol = K.Seq (K.Test (h, v), pol) in
	  HdrMap.fold f pr' (K.Test (h, v))

let seq_to_netkat (pol : seq) : K.pol =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if HdrMap.is_empty pol then
    K.Id
  else
    let (h, v) = HdrMap.min_binding pol in
    let pol' = HdrMap.remove h pol in
    let f h v pol' = K.Seq (K.Set (h, v), pol') in
    HdrMap.fold f pol' (K.Set (h, v))

let sum_to_netkat (pol : sum) : K.pol = 
  (* avoid printing trailing K.Drop if it is unnecessary *)
  if HdrValSet.is_empty pol then
    K.Drop
  else
    let f seq pol' = K.Par (seq_to_netkat seq, pol') in
    let seq = HdrValSet.min_elt pol in
    let pol' = HdrValSet.remove seq pol in
    HdrValSet.fold f pol' (seq_to_netkat seq)

let rec to_netkat (pol : local) : K.pol = match pol with
  | Action sum -> sum_to_netkat sum
  | ITE (pred, sum, pol') ->
    let pr = pred_to_netkat pred in
    K.Par (K.Seq (pr, sum_to_netkat sum),
    	     K.Seq (K.Neg pr, to_netkat pol'))