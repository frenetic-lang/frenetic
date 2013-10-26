type policy = NetKAT_Types.policy
type pred = NetKAT_Types.pred
type header = NetKAT_Types.header
type header_val = NetKAT_Types.header_val

(* BEGIN GENERIC HELPER FUNCTIONS ------------------------------------------- *)

let is_none m =
  match m with
    | None   -> true
    | Some _ -> false

let from_option a m =
  match m with
    | None   -> a
    | Some b -> b

let optional a f m =
  match m with
    | None   -> a
    | Some e -> f e

(* END GENERIC HELPER FUNCTIONS --------------------------------------------- *)


(* BEGIN DATA TYPES --------------------------------------------------------- *)

(* A link-free policy is—as the name suggests—a NetKAT policy that does not
 * contain links. 
 *)
type lf_policy =
  | Filter of pred
  | Mod of header * header_val
  | Par of lf_policy * lf_policy
  | Choice of lf_policy * lf_policy
  | Seq of lf_policy * lf_policy
  | Star of lf_policy

(* A separate datatype to represent links.
 *
 *   link = (sw1, pt1, sw2, pt2)
 *)
type link = header_val * header_val * header_val * header_val


(* A regular expression over link-free policy, link pairs. *)
type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty

(* An intermediate representation for the conversion from a NetKAT policy to a
 * regex. inter is a continuation with three cases. 
 *
 *   - The TP case can accept link-free policies and create a new continuation
 *   from that.
 *
 *   - The NL policy needs a link_provider to transition to another type of
 *   continuation, but can optionally accumulate link-free policies in the mean
 *   time. Accumulator is combined using the cstr function.
 *
 *   - An S is a fully-formed regular expression. At this point, the computation
 *   is done, as the continuation cannot accept new link-free policies or links.
 *)
type inter =
    | TP of link_provider
    | NL of link_consumer
    | S  of regex (* this is only in the star case *)
and cstr = lf_policy -> lf_policy -> lf_policy 
and link_provider = lf_policy option -> inter 
and link_consumer = cstr -> lf_policy option -> link_provider option -> inter

let seq (p : lf_policy) (q : lf_policy) : lf_policy = Seq(p, q)
let par (p : lf_policy) (q : lf_policy) : lf_policy = Par(p, q)

let mk_mcstr c mp q =
  optional q (fun x -> c x q) mp

(* Constructor for a link_consumer. Requires a link-free policy as the basis for
 * its accumulator so that if it passes its link-free policy to a link_provider,
 * it will not force the link_provider to transition to a regex.
 *)
let rec mk_nl (lfp : lf_policy) : link_consumer =
  fun cstr mlfp mlp ->
    let lfp' = mk_mcstr cstr mlfp lfp in
    match mlp with
      | None    -> NL(mk_nl lfp')
      | Some lp -> lp (Some(lfp'))

(* Constructor for link_provider. Requires a link and an optional link-free
 * policy that acts as an accumulator. If the link provider receives a `None`,
 * it will transition to a regex. If it receives `Some lfp` then it will
 * continue taking policies.
 *)
let rec mk_tp (l : link) (macc : lf_policy option) : link_provider =
  fun mlfq ->
    match mlfq with
      | None -> 
        let lfp = from_option (Filter(NetKAT_Types.True)) macc in
        S(Char(lfp, l))
      | Some lfq ->
        let lfp = optional lfq (fun acc -> Seq(lfq, acc)) macc in
        TP(mk_tp l (Some(lfp)))

(* END DATA TYPES ----------------------------------------------------------- *)

let regex_of_policy (p : policy) : regex =

  let rec of_mlink (mlp : link_provider option) : inter =
    match mlp with
      | None    -> NL(fun cstr mp mlp -> assert (is_none mp); of_mlink mlp)
      | Some lp -> lp None in

  let rec rpc (p : policy) : inter =
    begin match p with
      | NetKAT_Types.Filter(q) ->
        NL(mk_nl (Filter q))
      | NetKAT_Types.Mod(h, v) ->
        NL(mk_nl (Mod(h, v)))
      | NetKAT_Types.Link(sw1, pt1, sw2, pt2) ->
        TP(mk_tp (sw1, pt1, sw2, pt2) None)
      | NetKAT_Types.Seq(p1, p2) ->
        rpc_seq (rpc p1) (rpc p2)
      | NetKAT_Types.Par(p1, p2) ->
        rpc_par (rpc p1) (rpc p2)
      | NetKAT_Types.Choice(_, _) -> failwith "nyi"
      | NetKAT_Types.Star(q) ->
        S(Kleene(run (rpc q)))
    end

  and rpc_seq i j =
    begin match i, j with
      | TP f1, NL f2 -> NL(fun cstr mp ml -> rpc_seq (f1 mp) (f2 cstr None ml))
      | TP f1, i -> let r = run i in TP(fun mp -> rpc_seq (f1 mp) (S(r)))
      | NL f1, TP f2 -> f1 seq None (Some f2)
      | NL f1, NL f2 -> NL(fun cstr mp ml -> rpc_seq (f1 cstr mp None) (f2 cstr None ml))
      | NL f1, S  s2 -> failwith "Cat(NL, Star) can't be represented"
      | S  s1, i -> s_trans s1 i (fun x y -> Cat(x, y))
    end

  and rpc_par i j =
    begin match i, j with
      | TP f1, TP f2 -> TP(fun mp -> rpc_par (f1 mp) (f2 mp))
      | TP f1, NL f2 -> NL(fun cstr mp ml -> 
                            rpc_par (rpc_seq (f1 mp) (of_mlink ml)) (f2 cstr mp ml))
      | TP f1, S  s2 -> S(Alt(run (TP f1), s2))
      | NL f1, TP f2 -> NL(fun cstr mp ml ->
                            rpc_par (f1 cstr mp ml) (rpc_seq (f2 mp) (of_mlink ml)))
      | NL f1, NL f2 -> 
        NL(fun cstr mp ml ->
            match ml with
              | None   -> rpc_par (f1 cstr mp ml) (f2 cstr mp ml)
              | Some l -> f2 par None (Some(fun mlfp -> f1 par mlfp (Some(l)))))
      | NL f1, S  s2 -> NL(fun cstr mp ml ->
                            rpc_par (f1 par mp ml) (rpc_seq (S(s2)) (of_mlink ml)))
      | S  s1, i -> s_trans s1 i (fun x y -> Alt(x, y))
    end

  and s_trans (r : regex) (i : inter) c =
    begin match i with
      | NL f ->
        NL(fun cstr mp ml -> assert (is_none mp); S(c r (run (f cstr None ml))))
      | _ ->
        S(c r (run i))
    end

  and run_with (i : inter) (mp : lf_policy option) (mlp : link_provider option) : inter =
    begin match i with
      | TP f -> f mp
      | NL f -> f seq mp mlp
      | S  r -> S(r)
    end

  and run (i : inter) : regex =
    begin match run_with i None None with
      | TP _ -> failwith "should not happen"
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

