open BinNums
open Datatypes
open Peano

module Pos = 
 struct 
  type t = positive
  
  (** val succ : positive -> positive **)
  
  let rec succ = function
  | Coq_xI p -> Coq_xO (succ p)
  | Coq_xO p -> Coq_xI p
  | Coq_xH -> Coq_xO Coq_xH
  
  (** val add : positive -> positive -> positive **)
  
  let rec add x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xI (add p q)
       | Coq_xO q -> Coq_xO (add p q)
       | Coq_xH -> Coq_xI p)
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xO (succ q)
       | Coq_xO q -> Coq_xI q
       | Coq_xH -> Coq_xO Coq_xH)
  
  (** val add_carry : positive -> positive -> positive **)
  
  and add_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xI (add_carry p q)
       | Coq_xO q -> Coq_xO (add_carry p q)
       | Coq_xH -> Coq_xI (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xI (succ q)
       | Coq_xO q -> Coq_xO (succ q)
       | Coq_xH -> Coq_xI Coq_xH)
  
  (** val pred_double : positive -> positive **)
  
  let rec pred_double = function
  | Coq_xI p -> Coq_xI (Coq_xO p)
  | Coq_xO p -> Coq_xI (pred_double p)
  | Coq_xH -> Coq_xH
  
  (** val pred : positive -> positive **)
  
  let pred = function
  | Coq_xI p -> Coq_xO p
  | Coq_xO p -> pred_double p
  | Coq_xH -> Coq_xH
  
  (** val pred_N : positive -> coq_N **)
  
  let pred_N = function
  | Coq_xI p -> Npos (Coq_xO p)
  | Coq_xO p -> Npos (pred_double p)
  | Coq_xH -> N0
  
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
  
  (** val mask_rect : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rect f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val mask_rec : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rec f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val succ_double_mask : mask -> mask **)
  
  let succ_double_mask = function
  | IsNul -> IsPos Coq_xH
  | IsPos p -> IsPos (Coq_xI p)
  | IsNeg -> IsNeg
  
  (** val double_mask : mask -> mask **)
  
  let double_mask = function
  | IsPos p -> IsPos (Coq_xO p)
  | x0 -> x0
  
  (** val double_pred_mask : positive -> mask **)
  
  let double_pred_mask = function
  | Coq_xI p -> IsPos (Coq_xO (Coq_xO p))
  | Coq_xO p -> IsPos (Coq_xO (pred_double p))
  | Coq_xH -> IsNul
  
  (** val pred_mask : mask -> mask **)
  
  let pred_mask = function
  | IsPos q ->
    (match q with
     | Coq_xH -> IsNul
     | _ -> IsPos (pred q))
  | _ -> IsNeg
  
  (** val sub_mask : positive -> positive -> mask **)
  
  let rec sub_mask x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask p q)
       | Coq_xO q -> succ_double_mask (sub_mask p q)
       | Coq_xH -> IsPos (Coq_xO p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xH ->
      (match y with
       | Coq_xH -> IsNul
       | _ -> IsNeg)
  
  (** val sub_mask_carry : positive -> positive -> mask **)
  
  and sub_mask_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask_carry p q)
       | Coq_xO q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xH -> double_pred_mask p)
    | Coq_xH -> IsNeg
  
  (** val sub : positive -> positive -> positive **)
  
  let sub x y =
    match sub_mask x y with
    | IsPos z -> z
    | _ -> Coq_xH
  
  (** val mul : positive -> positive -> positive **)
  
  let rec mul x y =
    match x with
    | Coq_xI p -> add y (Coq_xO (mul p y))
    | Coq_xO p -> Coq_xO (mul p y)
    | Coq_xH -> y
  
  (** val iter : positive -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let rec iter n f x =
    match n with
    | Coq_xI n' -> f (iter n' f (iter n' f x))
    | Coq_xO n' -> iter n' f (iter n' f x)
    | Coq_xH -> f x
  
  (** val pow : positive -> positive -> positive **)
  
  let pow x y =
    iter y (mul x) Coq_xH
  
  (** val square : positive -> positive **)
  
  let rec square = function
  | Coq_xI p0 -> Coq_xI (Coq_xO (add (square p0) p0))
  | Coq_xO p0 -> Coq_xO (Coq_xO (square p0))
  | Coq_xH -> Coq_xH
  
  (** val div2 : positive -> positive **)
  
  let div2 = function
  | Coq_xI p0 -> p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH
  
  (** val div2_up : positive -> positive **)
  
  let div2_up = function
  | Coq_xI p0 -> succ p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH
  
  (** val size_nat : positive -> int **)
  
  let rec size_nat = function
  | Coq_xI p0 -> succ (size_nat p0)
  | Coq_xO p0 -> succ (size_nat p0)
  | Coq_xH -> succ 0
  
  (** val size : positive -> positive **)
  
  let rec size = function
  | Coq_xI p0 -> succ (size p0)
  | Coq_xO p0 -> succ (size p0)
  | Coq_xH -> Coq_xH
  
  (** val compare_cont : positive -> positive -> int -> int **)
  
  let rec compare_cont x y r =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> compare_cont p q r
       | Coq_xO q -> compare_cont p q 1
       | Coq_xH -> 1)
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> compare_cont p q (-1)
       | Coq_xO q -> compare_cont p q r
       | Coq_xH -> 1)
    | Coq_xH ->
      (match y with
       | Coq_xH -> r
       | _ -> (-1))
  
  (** val compare : positive -> positive -> int **)
  
  let compare x y =
    compare_cont x y 0
  
  (** val min : positive -> positive -> positive **)
  
  let min p p' =
    match compare p p' with
    | 1 -> p'
    | _ -> p
  
  (** val max : positive -> positive -> positive **)
  
  let max p p' =
    match compare p p' with
    | 1 -> p
    | _ -> p'
  
  (** val eqb : positive -> positive -> bool **)
  
  let rec eqb p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> eqb p0 q0
       | _ -> false)
    | Coq_xO p0 ->
      (match q with
       | Coq_xO q0 -> eqb p0 q0
       | _ -> false)
    | Coq_xH ->
      (match q with
       | Coq_xH -> true
       | _ -> false)
  
  (** val leb : positive -> positive -> bool **)
  
  let leb x y =
    match compare x y with
    | 1 -> false
    | _ -> true
  
  (** val ltb : positive -> positive -> bool **)
  
  let ltb x y =
    match compare x y with
    | (-1) -> true
    | _ -> false
  
  (** val sqrtrem_step :
      (positive -> positive) -> (positive -> positive) -> (positive * mask)
      -> positive * mask **)
  
  let sqrtrem_step f g = function
  | (s, y) ->
    (match y with
     | IsPos r ->
       let s' = Coq_xI (Coq_xO s) in
       let r' = g (f r) in
       if leb s' r'
       then ((Coq_xI s), (sub_mask r' s'))
       else ((Coq_xO s), (IsPos r'))
     | _ -> ((Coq_xO s), (sub_mask (g (f Coq_xH)) (Coq_xO (Coq_xO Coq_xH)))))
  
  (** val sqrtrem : positive -> positive * mask **)
  
  let rec sqrtrem = function
  | Coq_xI p0 ->
    (match p0 with
     | Coq_xI p1 ->
       sqrtrem_step (fun x -> Coq_xI x) (fun x -> Coq_xI x) (sqrtrem p1)
     | Coq_xO p1 ->
       sqrtrem_step (fun x -> Coq_xO x) (fun x -> Coq_xI x) (sqrtrem p1)
     | Coq_xH -> (Coq_xH, (IsPos (Coq_xO Coq_xH))))
  | Coq_xO p0 ->
    (match p0 with
     | Coq_xI p1 ->
       sqrtrem_step (fun x -> Coq_xI x) (fun x -> Coq_xO x) (sqrtrem p1)
     | Coq_xO p1 ->
       sqrtrem_step (fun x -> Coq_xO x) (fun x -> Coq_xO x) (sqrtrem p1)
     | Coq_xH -> (Coq_xH, (IsPos Coq_xH)))
  | Coq_xH -> (Coq_xH, IsNul)
  
  (** val sqrt : positive -> positive **)
  
  let sqrt p =
    fst (sqrtrem p)
  
  (** val gcdn : int -> positive -> positive -> positive **)
  
  let rec gcdn n a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      Coq_xH)
      (fun n0 ->
      match a with
      | Coq_xI a' ->
        (match b with
         | Coq_xI b' ->
           (match compare a' b' with
            | 0 -> a
            | (-1) -> gcdn n0 (sub b' a') a
            | 1 -> gcdn n0 (sub a' b') b)
         | Coq_xO b0 -> gcdn n0 a b0
         | Coq_xH -> Coq_xH)
      | Coq_xO a0 ->
        (match b with
         | Coq_xI p -> gcdn n0 a0 b
         | Coq_xO b0 -> Coq_xO (gcdn n0 a0 b0)
         | Coq_xH -> Coq_xH)
      | Coq_xH -> Coq_xH)
      n
  
  (** val gcd : positive -> positive -> positive **)
  
  let gcd a b =
    gcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val ggcdn :
      int -> positive -> positive -> positive * (positive * positive) **)
  
  let rec ggcdn n a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> (Coq_xH, (a,
      b)))
      (fun n0 ->
      match a with
      | Coq_xI a' ->
        (match b with
         | Coq_xI b' ->
           (match compare a' b' with
            | 0 -> (a, (Coq_xH, Coq_xH))
            | (-1) ->
              let (g, p) = ggcdn n0 (sub b' a') a in
              let (ba, aa) = p in (g, (aa, (add aa (Coq_xO ba))))
            | 1 ->
              let (g, p) = ggcdn n0 (sub a' b') b in
              let (ab, bb) = p in (g, ((add bb (Coq_xO ab)), bb)))
         | Coq_xO b0 ->
           let (g, p) = ggcdn n0 a b0 in
           let (aa, bb) = p in (g, (aa, (Coq_xO bb)))
         | Coq_xH -> (Coq_xH, (a, Coq_xH)))
      | Coq_xO a0 ->
        (match b with
         | Coq_xI p ->
           let (g, p0) = ggcdn n0 a0 b in
           let (aa, bb) = p0 in (g, ((Coq_xO aa), bb))
         | Coq_xO b0 -> let (g, p) = ggcdn n0 a0 b0 in ((Coq_xO g), p)
         | Coq_xH -> (Coq_xH, (a, Coq_xH)))
      | Coq_xH -> (Coq_xH, (Coq_xH, b)))
      n
  
  (** val ggcd : positive -> positive -> positive * (positive * positive) **)
  
  let ggcd a b =
    ggcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val coq_Nsucc_double : coq_N -> coq_N **)
  
  let coq_Nsucc_double = function
  | N0 -> Npos Coq_xH
  | Npos p -> Npos (Coq_xI p)
  
  (** val coq_Ndouble : coq_N -> coq_N **)
  
  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (Coq_xO p)
  
  (** val coq_lor : positive -> positive -> positive **)
  
  let rec coq_lor p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xO q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xH -> p)
    | Coq_xO p0 ->
      (match q with
       | Coq_xI q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xO q0 -> Coq_xO (coq_lor p0 q0)
       | Coq_xH -> Coq_xI p0)
    | Coq_xH ->
      (match q with
       | Coq_xO q0 -> Coq_xI q0
       | _ -> q)
  
  (** val coq_land : positive -> positive -> coq_N **)
  
  let rec coq_land p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> coq_Nsucc_double (coq_land p0 q0)
       | Coq_xO q0 -> coq_Ndouble (coq_land p0 q0)
       | Coq_xH -> Npos Coq_xH)
    | Coq_xO p0 ->
      (match q with
       | Coq_xI q0 -> coq_Ndouble (coq_land p0 q0)
       | Coq_xO q0 -> coq_Ndouble (coq_land p0 q0)
       | Coq_xH -> N0)
    | Coq_xH ->
      (match q with
       | Coq_xO q0 -> N0
       | _ -> Npos Coq_xH)
  
  (** val ldiff : positive -> positive -> coq_N **)
  
  let rec ldiff p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> coq_Ndouble (ldiff p0 q0)
       | Coq_xO q0 -> coq_Nsucc_double (ldiff p0 q0)
       | Coq_xH -> Npos (Coq_xO p0))
    | Coq_xO p0 ->
      (match q with
       | Coq_xI q0 -> coq_Ndouble (ldiff p0 q0)
       | Coq_xO q0 -> coq_Ndouble (ldiff p0 q0)
       | Coq_xH -> Npos p)
    | Coq_xH ->
      (match q with
       | Coq_xO q0 -> Npos Coq_xH
       | _ -> N0)
  
  (** val coq_lxor : positive -> positive -> coq_N **)
  
  let rec coq_lxor p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> coq_Ndouble (coq_lxor p0 q0)
       | Coq_xO q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | Coq_xH -> Npos (Coq_xO p0))
    | Coq_xO p0 ->
      (match q with
       | Coq_xI q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | Coq_xO q0 -> coq_Ndouble (coq_lxor p0 q0)
       | Coq_xH -> Npos (Coq_xI p0))
    | Coq_xH ->
      (match q with
       | Coq_xI q0 -> Npos (Coq_xO q0)
       | Coq_xO q0 -> Npos (Coq_xI q0)
       | Coq_xH -> N0)
  
  (** val shiftl_nat : positive -> int -> positive **)
  
  let shiftl_nat p n =
    nat_iter n (fun x -> Coq_xO x) p
  
  (** val shiftr_nat : positive -> int -> positive **)
  
  let shiftr_nat p n =
    nat_iter n div2 p
  
  (** val shiftl : positive -> coq_N -> positive **)
  
  let shiftl p = function
  | N0 -> p
  | Npos n0 -> iter n0 (fun x -> Coq_xO x) p
  
  (** val shiftr : positive -> coq_N -> positive **)
  
  let shiftr p = function
  | N0 -> p
  | Npos n0 -> iter n0 div2 p
  
  (** val testbit_nat : positive -> int -> bool **)
  
  let rec testbit_nat p n =
    match p with
    | Coq_xI p0 ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ ->
         true)
         (fun n' ->
         testbit_nat p0 n')
         n)
    | Coq_xO p0 ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ ->
         false)
         (fun n' ->
         testbit_nat p0 n')
         n)
    | Coq_xH ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ ->
         true)
         (fun n0 ->
         false)
         n)
  
  (** val testbit : positive -> coq_N -> bool **)
  
  let rec testbit p n =
    match p with
    | Coq_xI p0 ->
      (match n with
       | N0 -> true
       | Npos n0 -> testbit p0 (pred_N n0))
    | Coq_xO p0 ->
      (match n with
       | N0 -> false
       | Npos n0 -> testbit p0 (pred_N n0))
    | Coq_xH ->
      (match n with
       | N0 -> true
       | Npos p0 -> false)
  
  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)
  
  let rec iter_op op p a =
    match p with
    | Coq_xI p0 -> op a (iter_op op p0 (op a a))
    | Coq_xO p0 -> iter_op op p0 (op a a)
    | Coq_xH -> a
  
  (** val to_nat : positive -> int **)
  
  let to_nat x =
    iter_op plus x (succ 0)
  
  (** val of_nat : int -> positive **)
  
  let rec of_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      Coq_xH)
      (fun x ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ ->
        Coq_xH)
        (fun n0 ->
        succ (of_nat x))
        x)
      n
  
  (** val of_succ_nat : int -> positive **)
  
  let rec of_succ_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      Coq_xH)
      (fun x ->
      succ (of_succ_nat x))
      n
 end

