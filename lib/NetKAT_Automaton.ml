type policy = NetKAT_Types.policy
type pred = NetKAT_Types.pred
type header = NetKAT_Types.header
type header_val = NetKAT_Types.header_val

type lf_policy =
  | Filter of pred
  | Mod of header * header_val
  | Par of lf_policy * lf_policy
  | Choice of lf_policy * lf_policy
  | Seq of lf_policy * lf_policy
  | Star of lf_policy

type link = header_val * header_val * header_val * header_val

type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty

type inter =
  | TP of (lf_policy option -> inter)
  | NL of (lf_policy option -> link option -> inter)
  | S of regex (* this is only in the star case *)

let is_none m =
  match m with
    | None   -> true
    | Some _ -> false

let regex_of_policy (p : policy) : regex =

  let of_mlink (ml : link option) : regex =
    match ml with
      | None   -> failwith "NL forced without a link"
      | Some l -> Char(Filter NetKAT_Types.True, l) in
  let mk_seq (mp : lf_policy option) (q : lf_policy) : lf_policy=
    match mp with
      | None -> q
      | Some p -> Seq(p, q) in
  let mp_seq (mp : lf_policy option) (mq : lf_policy option) : lf_policy option =
    match mq with
      | Some q -> Some (mk_seq mp q)
      | None   -> mp in

  let rec rpc (p : policy) : inter = 
    begin match p with
      | NetKAT_Types.Filter(q) ->
        NL(fun mp ml -> e_nl_seq_trans (Some(mk_seq mp (Filter q))) ml)
      | NetKAT_Types.Mod(h, v) ->
        NL(fun mp ml -> e_nl_seq_trans (Some(mk_seq mp (Mod(h, v)))) ml)
      | NetKAT_Types.Link(sw1, pt1, sw2, pt2) ->
        TP(fun mq -> e_tp_seq_trans mq (sw1, pt1, sw2, pt2))
      | NetKAT_Types.Seq(p1, p2) ->
        begin match rpc p1, rpc p2 with
          | TP f1, TP f2 -> TP(fun mp    -> Cat(f1 mp, run f2))
          | TP f1, NL f2 -> NL(fun mp ml -> Cat(f1 mp, run f2))
          | TP f1, S  s2 -> TP(fun mp    -> Cat(f1 mp, s2))
          | NL f1, TP f2 -> TP(failwith "nyi")
          | NL f1, NL f2 -> NL(failwith "nyi")
          | NL f1, S  s2 -> failwith "Cat(NL, Star) can't be represented"
          | S  s1, i -> s_trans s1 i (fun x y -> Cat(x, y))
        end
      | NetKAT_Types.Par(p1, p2) ->
        begin match rpc p1, rpc p2 with
          | TP f1, TP f2 -> TP(fun mp    -> Alt(f1 mp, f2 mp))
          | TP f1, NL f2 -> NL(fun mp ml -> Alt(Cat(f1 mp, of_mlink ml), f2 mp ml))
          | TP f1, S  s2 -> S(Alt(run f1, s2))
          | NL f1, TP f2 -> NL(fun mp ml -> Alt(f1 mp ml, Cat(f1 mp, of_mlink ml)))
          | NL f1, NL f2 -> NL(fun mp ml -> Alt(f1 mp ml, f2 mp ml))
          | NL f1, S  s2 -> NL(failwith "nyi")
          | S  s1, i -> s_trans s1 i (fun x y -> Alt(x, y))
        end
      | NetKAT_Types.Star(q) -> S(Kleene(run (rpc q)))
    end

  and e_nl_seq_trans (mq : lf_policy option) (ml : link option) =
    begin match ml with
      | None -> 
        NL(fun mp ml' -> e_nl_seq_trans (mp_seq mp mq) ml')
      | Some(sw1, pt1, sw2, pt2) ->
        TP(fun mp -> e_tp_seq_trans mp (sw1, pt1, sw2, pt2))
    end

  and e_tp_seq_trans (mq : lf_policy option) (l : link) =
    begin match mq with
      | None ->
        S(Char(Filter NetKAT_Types.True, l))
      | Some q ->
        TP(fun mp -> e_tp_seq_trans (Some(mk_seq mp q)) l)
    end

  and s_trans (r : regex) (i : inter) c =
    begin match i with
      | NL _ ->
        NL(fun mp ml -> assert (is_none mp); S(c r (run (run_with i ml))))
      | _ ->
        S(c r (run i))
    end

  and run_with (i : inter) (ml : link option) : inter =
    begin match i with
      | TP f -> f None
      | NL f -> f None ml
      | S  r -> S(r)
    end

  and run (i : inter) : regex =
    begin match run_with i None with
      | TP f -> failwith "should not happen"
      | NL _ -> failwith "need a link in there"
      | S  r -> r
    end in

  run (rpc p)


(*
 * Helper function for `regex_of_policy`. Straight up replace constructors from
 * this module with the identically-named constructors from `NetKAT_Types`.
 *)
let rec lf_policy_to_policy (lfp : lf_policy) : policy =
  match lfp with
    | Filter(p) -> NetKAT_Types.Filter(p)
    | Mod(h, v) -> NetKAT_Types.Mod(h, v)
    | Par(p1, p2) ->
      NetKAT_Types.Par(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Choice(p1, p2) ->
      NetKAT_Types.Choice(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Seq(p1, p2) ->
      NetKAT_Types.Seq(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Star(p) ->
      NetKAT_Types.Star(lf_policy_to_policy p)

(* Helper function for `regex_of_policy`. Take care of link translation *)
let link_to_policy ((sw1, pt1, sw2, pt2) : link) : policy =
  NetKAT_Types.Link(sw1, pt1, sw2, pt2)

let rec regex_to_policy (r : regex) : policy =
  match r with
    | Char(lfp, l) ->
      NetKAT_Types.Seq(lf_policy_to_policy lfp, link_to_policy l)
    | Alt(r1, r2) ->
      NetKAT_Types.Choice(regex_to_policy r1, regex_to_policy r2)
    | Cat(r1, r2) ->
      NetKAT_Types.Seq(regex_to_policy r1, regex_to_policy r2)
    | Kleene(r) ->
      NetKAT_Types.Star(regex_to_policy r)
    | Empty -> NetKAT_Types.Filter(NetKAT_Types.True)
