open Types
open Pretty

(* JNF: why are we duplicating so much code? *)

(* i;(p;t)^*;e *)

type vtag = int
type vheader = 
  | Field of Types.header
  | Tag of vtag

type virtual_pol =
  | VFilter of pred
  | VTest of vtag*header_val
  | VMod of vheader * header_val
    (* switch, port -> switch, port *)
  | VLink of header_val*header_val*header_val*header_val
  | VPar of virtual_pol * virtual_pol
  | VChoice of virtual_pol * virtual_pol
  | VSeq of virtual_pol * virtual_pol
  | VStar of virtual_pol
      
type restricted_pol = virtual_pol * virtual_pol * virtual_pol * virtual_pol

module Formatting = struct

  open Format

  let header = format_header

  let vheader (fmt : formatter) (h : vheader) : unit = match h with
    | Field h' -> format_header fmt h'
    | Tag h -> fprintf fmt "%s" (string_of_int h)

    (* The type of the immediately surrounding context, which guides parenthesis-
       insertion. *)
    (* JNF: YES. This is the Right Way to pretty print. *)
  type context = SEQ | PAR | CHOICE | STAR | NEG | PAREN

  let rec pred (cxt : context) (fmt : formatter) (pr : pred) : unit = 
    match pr with
      | True -> 
        fprintf fmt "@[true@]"
      | False -> 
        fprintf fmt "@[false@]"
      | (Test (h, v)) -> 
        fprintf fmt "@[%a = %a@]" header h VInt.format v
      | Neg p' -> 
        begin match cxt with
          | PAREN -> fprintf fmt "@[!%a@]" (pred NEG) p'
          | _ -> fprintf fmt "@[!@[(%a)@]@]" (pred PAREN) p'
        end
      | Or (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR -> fprintf fmt "@[%a | %a@]" (pred PAR) p1 (pred PAR) p2
          | _ -> fprintf fmt "@[(@[%a | %a@])@]" (pred PAR) p1 (pred PAR) p2
        end
      | And (p1, p2) -> 
        begin match cxt with
          | PAREN
          | SEQ
          | CHOICE
          | PAR -> fprintf fmt "@[%a ; %a@]" (pred SEQ) p1 (pred SEQ) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (pred SEQ) p1 (pred SEQ) p2
        end

  let rec vpol (cxt : context) (fmt : formatter) (p : virtual_pol) : unit =
    match p with
      | VFilter pr -> 
        pred cxt fmt pr
      | VTest (h,v) -> fprintf fmt "@[%a = %a@]" vheader (Tag h) VInt.format v
      | VMod (h, v) -> 
        fprintf fmt "@[%a <- %a@]" vheader h VInt.format v
      | VStar p' -> 
        begin match cxt with
          | PAREN 
	  | STAR ->  fprintf fmt "@[%a*@]" (vpol STAR) p' 
          | _ -> fprintf fmt "@[@[(%a)*@]@]" (vpol PAREN) p'
        end
      | VLink (sw,pt,sw',pt') ->
        let fmter = VInt.format in
        fprintf fmt "@[(%a@@%a) -> (%a@%a)@]" fmter sw fmter pt fmter sw' fmter pt'
      | VPar (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR -> fprintf fmt "@[%a | %a@]" (vpol PAR) p1 (vpol PAR) p2
          | _ -> fprintf fmt "@[(@[%a | %a@])@]" (vpol PAR) p1 (vpol PAR) p2
        end
      | VChoice (p1, p2) -> 
        begin match cxt with
          | PAREN
          | CHOICE -> fprintf fmt "@[%a + %a@]" (vpol CHOICE) p1 (vpol CHOICE) p2
          | _ -> fprintf fmt "@[(@[%a + %a@])@]" (vpol CHOICE) p1 (vpol CHOICE) p2
        end
      | VSeq (p1, p2) -> 
        begin match cxt with
          | PAREN
          | SEQ
          | CHOICE
          | PAR -> fprintf fmt "@[%a ; %a@]" (vpol SEQ) p1 (vpol SEQ) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (vpol SEQ) p1 (vpol SEQ) p2
        end

end

let make_string_of formatter x =
  let open Format in
      let buf = Buffer.create 100 in
      let fmt = formatter_of_buffer buf in
      pp_set_margin fmt 80;
      formatter fmt x;
      fprintf fmt "@?";
      Buffer.contents buf

let string_of_vint = make_string_of VInt.format 

let string_of_header = make_string_of Formatting.header
let string_of_vheader = make_string_of Formatting.vheader

let format_vpolicy = Formatting.vpol Formatting.PAREN

let string_of_vpolicy = make_string_of format_vpolicy

let vheader_count = ref 0

let gen_header () =
  incr vheader_count;
  !vheader_count

  (* Compilation story: we have an unlimited number of header fields we
     can allocate on demand. Each header field has a specific number of
     entries. At a later stage, we inject the multiple header fields into a
     single header that allows arbitrary bitmasking. There should be some
     analysis/optimizations we can do (ala the earlier slices compiler) to
     reduce the number of unique entries required.

     Alternatively, lacking a bitmaskable field, we could simply expand
     all possible combinations of values and match/set them
     appropriately. We'd need some pretty good analysis to keep this
     from exploding.
  *)
let rec seqList ls =
  match ls with
    | [] -> VFilter True
    | l :: ls -> VSeq(l, seqList ls)

let rec parList ls =
  match ls with
    | [] -> VFilter False
    | l :: ls -> VPar(l, parList ls)


let rec dehopify (p : policy) : restricted_pol =
  match p with
    | Filter pr -> 
      VFilter(pr),
      VFilter False,
      VFilter False, 
      VFilter True
    | Mod (h', v) -> 
      VMod(Field h', v),
      VFilter False,
      VFilter False, 
      VFilter True
    | Link (sw1,p1,sw2,p2) -> 
      VSeq(VFilter(Test(Switch, sw1)),
           VFilter(Test(Header SDN_Types.InPort, p1))),
      VFilter True,
      VLink(sw1,p1,sw2,p2),
      VSeq(VFilter(Test(Switch, sw2)),
        VFilter(Test(Header SDN_Types.InPort, p2)))
    | Par (p,q) -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let i_q,s_q,t_q,e_q = dehopify q in
      let h = gen_header () in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      VPar(VSeq(VMod(Tag h,h0), i_p),
           VSeq(VMod(Tag h,h1), i_q)),
      VPar(VSeq(VTest(h,h0),s_p),
           VSeq(VTest(h,h1),s_q)),
      VPar(t_p,t_q),
      VPar(VSeq(VTest(h,h0), e_p),
           VSeq(VTest(h,h1), e_q))
    | Choice (p,q) -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let i_q,s_q,t_q,e_q = dehopify q in
      let h = gen_header () in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      VChoice(VSeq(VMod(Tag h,h0), i_p),
           VSeq(VMod(Tag h,h1), i_q)),
      VPar(VSeq(VTest(h,h0),s_p),
           VSeq(VTest(h,h1),s_q)),
      VPar(t_p,t_q),
      VPar(VSeq(VTest(h,h0), e_p),
           VSeq(VTest(h,h1), e_q))
    | Seq (p,q) -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let i_q,s_q,t_q,e_q = dehopify q in
      let h = gen_header () in
      let h1 = VInt.Int16 0 in
      let h2 = VInt.Int16 1 in
      VSeq(VMod(Tag h,h1),
           i_p),
      parList [ seqList [VTest(h,h1);
                         s_p];
               seqList [VTest(h,h1);
                        e_p;
                        i_q;
                        s_q;
                        VMod(Tag h,h2)];
               seqList [VTest(h,h2);
                        s_q]],
      VPar(t_p,t_q),
      parList [seqList [VTest(h,h1);
                        e_p;
                        i_q;
                        e_q];
               VSeq(VTest(h, h2), e_q)]
    | Star p -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let h = gen_header () in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      parList [VMod(Tag h,h0);
               VSeq(VMod(Tag h,h1), i_p)],
      seqList [VTest(h,h1);
               VStar(VSeq (e_p,i_p));
               s_p],
      t_p,
      VPar(VSeq(VTest(h,h1),
                e_p),
           VTest(h,h0))

module Optimization = struct

  (* Optimizations Observation: Virtual headers are semantically
     meaningless if they aren't matched in the NetKAT code itself. Thus,
     if we optimize away as many matches as possible, we can then drop
     any headers that are not matched on.
  *)

  (* reduces policies by using identities (drop;p = drop, etc) *)
  let rec simplify_vpol p =
    match p with
      | VSeq(VFilter False, _) -> VFilter False
      | VSeq(_, VFilter False) -> VFilter False
      | VSeq(VFilter True, p) -> simplify_vpol p
      | VSeq(p, VFilter True) -> simplify_vpol p
      | VSeq(p, q) -> let p' = simplify_vpol p in
                      let q' = simplify_vpol q in
                      if p' = p & q' = q then
                        VSeq(p',q')
                      else
                        simplify_vpol (VSeq(p',q'))
      | VPar(VFilter False, p) -> simplify_vpol p
      | VPar(p, VFilter False) -> simplify_vpol p
      | VPar(p, VPar(q,r)) ->
        if p = q then simplify_vpol (VPar(q,r))
        else let p' = simplify_vpol p in
             if p' = p then
               VPar(p, simplify_vpol (VPar(q,r)))
             else
               simplify_vpol (VPar(p', VPar(q,r)))
      | VPar(p,q) -> 
        if p = q then p
        else
          let p' = simplify_vpol p in
          let q' = simplify_vpol q in
          if p' = p & q' = q then
            VPar(p',q')
          else
            simplify_vpol (VPar(p',q'))
      | VChoice(p, VChoice(q,r)) ->
        if p = q then simplify_vpol (VChoice(q,r))
        else let p' = simplify_vpol p in
             if p' = p then
               VChoice(p, simplify_vpol (VChoice(q,r)))
             else
               simplify_vpol (VChoice(p', VChoice(q,r)))
      | VChoice(p,q) ->
        if p = q then p
        else
          let p' = simplify_vpol p in
          let q' = simplify_vpol q in
          if p' = p & q' = q
          then VChoice(p',q')
          else simplify_vpol (VChoice(p',q'))
      | VStar(VFilter False) -> VFilter True
      | VStar(VFilter True) -> VFilter True
      | VStar(p) -> 
        begin
          match simplify_vpol p with
            | VFilter False -> VFilter True
            | VFilter True -> VFilter True
            | p -> VStar p
        end
      | _ -> p

  (* Linearizes seqs (i.e. Seq(Seq(a,b),c) => Seq(a,Seq(b,c) *)
  let rec vpol_to_linear_vpol p = 
    match p with
      (* (p | q) | r = p (q | r) *)
      | VPar (VPar (p,q), r) ->
        vpol_to_linear_vpol (VPar (p, VPar(q,r)))
      | VPar (p, q) ->
        let p' = vpol_to_linear_vpol p in
        let q' = vpol_to_linear_vpol q in
        if p' = p && q' = q then
          VPar(p,q)
        else vpol_to_linear_vpol (VPar(p',q'))
      (* (p;q);r = p;(q;r) *)
      | VSeq (VSeq (p,q), r) ->
        vpol_to_linear_vpol (VSeq (p, VSeq (q,r)))
      | VSeq (p,q) ->
        let p' = vpol_to_linear_vpol p in
        let q' = vpol_to_linear_vpol q in
        if p' = p && q' = q then
          VSeq(p,q)
        else vpol_to_linear_vpol (VSeq(p',q'))
      | VChoice(p,q) ->
        let p' = vpol_to_linear_vpol p in
        let q' = vpol_to_linear_vpol q in
        if p' = p && q' = q then
          VChoice(p,q)
        else vpol_to_linear_vpol (VChoice(p',q'))
      | VStar p -> VStar (vpol_to_linear_vpol p)
      | _ -> p

  let reorder_seq p q =
    match p,q with
      | VMod(Tag h,v), VMod(Tag h',v') ->
        if h' < h then q,p else p,q
      | VMod(h,v), VMod(Tag h',v') ->
        q,p
      | VMod(Tag h,v), VMod(h',v') ->
        p,q
      | VMod(h,v), VMod(h',v') ->
        if h' < h then q,p else p,q
      | VMod(Tag h,v), VTest(h',v') ->
        if h' < h then q,p else p,q
      | VTest(h,v),VMod(Tag h',v') ->
        if h' < h then q,p else p,q
      | VMod(h',v'),VTest(h,v) ->
        q,p
      | VTest(h,v), VTest(h',v') ->
        if h' < h then q,p else p,q
      (* Because preds can't match on tags, they always commute with a tag mod *)
      | VFilter a, VMod (Tag h, v) ->
        q,p
      (* Because preds can't match on tags, they always commute with a tag test *)
      | VFilter a, VTest (h, v) ->
        q,p
      | _ -> p,q

  (* Normalizes sequences by reordering mods/matches by the headers:
     h <- v; h' <- v' = h' <- v'; h <- v if h <> h'
     Assumes linearized seq
     Equivalent to bubble sort. No, I don't care.
  *)
  let rec normalize_seq p =
    match p with
      | VSeq(p, VSeq(q,r)) ->
        let p', q' = reorder_seq p q in
        if p' = p then
          let s = VSeq(q, r) in
          let s' = normalize_seq s in
          if s = s' then VSeq(p, s)
          else normalize_seq (VSeq(p, s'))
        else
          normalize_seq (VSeq(p',VSeq(q',r)))
      | VSeq(p,q) ->
        let p',q' = reorder_seq p q in
        VSeq(p',q')
      | VPar(p,q) -> let p' = normalize_seq p in
                     let q' = normalize_seq q in
                     if q' < p' then
                       normalize_seq (VPar(q',p'))
                     else
                       VPar(p',q')
      | VChoice(p,q) -> let p' = normalize_seq p in
                        let q' = normalize_seq q in
                        if q' < p' then
                          normalize_seq (VChoice(q',p'))
                        else
                          VChoice(p',q')
      | VStar(p) -> VStar(normalize_seq p)
      | _ -> p

  (* Dataflow-esque analysis that tracks current possible values for
     header fields. Useful for dead-code elimination/simplification *)
        
  module ValSet = Set.Make (struct
    type t = header_val
    let compare = Pervasives.compare
  end)

  type dataflow_vals =
    | NoVal
    | AnyVal
    | ExactVal of header_val
    | PossibleVals of ValSet.t

  let string_of_dataflow_val v = match v with
    | NoVal -> "NoVal"
    | AnyVal -> "AnyVal"
    | ExactVal v' -> Printf.sprintf "ExactVal %s" (string_of_vint v')
    | PossibleVals vs -> Printf.sprintf "PossibleVals {%s}" (String.concat ";" (List.map string_of_vint (ValSet.elements vs)))

  let join dv dv' =
    match dv,dv' with
      | NoVal, _ -> dv'
      | _, NoVal -> dv
      | AnyVal, _ -> AnyVal
      | _, AnyVal -> AnyVal
      | ExactVal v, ExactVal v' -> if v = v' then ExactVal v else PossibleVals (ValSet.add v (ValSet.singleton v'))
      | ExactVal v, PossibleVals v' -> PossibleVals (ValSet.add v v')
      | PossibleVals v, ExactVal v' -> PossibleVals (ValSet.add v' v)
      | PossibleVals v, PossibleVals v' -> PossibleVals (ValSet.union v v')

  let meet dv dv' =
    match dv,dv' with
      | NoVal, _ -> NoVal
      | _, NoVal -> NoVal
      | AnyVal, _ -> dv'
      | _, AnyVal -> dv
      | ExactVal v, ExactVal v' -> if v = v' then ExactVal v else NoVal
      | ExactVal v, PossibleVals v' -> if ValSet.mem v v' then ExactVal v else NoVal
      | PossibleVals v, ExactVal v' -> if ValSet.mem v' v then ExactVal v' else NoVal
      | PossibleVals v, PossibleVals v' -> 
        let u = (ValSet.inter v v') in
        begin
          match ValSet.cardinal u with
            | 0 -> NoVal
            | 1 -> ExactVal (ValSet.choose u)
            | _ -> PossibleVals u
        end

  (* We keep our alist sorted by keys. This doesn't speed up lookup
     (much), but it does reduce merging from O(n^2) to O(n) 
     Sorted in descending order
  *)
  let lookup' default alist k =
    List.fold_left (fun acc (k',v') -> if k' = k then v' else acc) default alist

  let lookup = lookup' AnyVal

  let rec insert alist k v = match alist with
    | [] -> [(k, v)]
    | (k',v') :: alist -> let cmp = Pervasives.compare k k' in
                          if cmp < 0 then
                            (k',v') :: insert alist k v
                          else if cmp = 0 then
                            (k, v) :: alist
                          else
                            (k,v) :: (k',v') :: alist
                              
  let rec merge' merger default alist1 alist2 = match alist1,alist2 with
    | [],_ -> List.map (fun (h, v) -> (h, merger v default)) alist2
    | _, [] -> List.map (fun (h, v) -> (h, merger v default)) alist1
    | (k,v):: alist1, (k',v') :: alist2 ->
      let cmp = Pervasives.compare k k' in
      if cmp < 0 then
        (k', merger v' default) :: (merge' merger default ((k,v):: alist1) alist2)
      else if cmp = 0 then
        (k, merger v v') :: (merge' merger default alist1 alist2)
      else
        (k, merger v default) :: (merge' merger default alist1 ((k',v') :: alist2))

  let merge a b = merge' join AnyVal a b

  let star_merge a b = merge' join NoVal a b

  let dataflow_val_eq v1 v2 = match v1,v2 with
    | AnyVal,AnyVal -> true
    | NoVal, NoVal -> true
    | ExactVal v1, ExactVal v2 -> v1 = v2
    | PossibleVals v1s, PossibleVals v2s -> ValSet.equal v1s v2s
    | _ -> false

  let rec val_list_eq ls1 ls2 =
    match ls1, ls2 with
      | [],[] -> true
      | [],_ -> false
      | _,[] -> false
      | (h,a) :: ls1, (h',b) :: ls2 -> h = h' && dataflow_val_eq a b && val_list_eq ls1 ls2

  (* Removes tests shadowed by mods: (h <- v; ...; h = v) and (h <- v;
     ...; h = v') where ".." doesn't contain a mod to 'h' and v' <> v *)
  let rec remove_dead_matches' p tbl =
    match p with
      | VMod(Tag h, v) -> begin
        match lookup tbl h with
          | ExactVal v'  -> if v' = v then
              VFilter True, tbl
            else
              VMod(Tag h, v), insert tbl h (ExactVal v)
          | _ -> VMod(Tag h, v), insert tbl h (ExactVal v)
      end
      | VTest(h,v) -> 
        begin
          match lookup tbl h with
            | ExactVal v' -> if v' = v then
                VFilter True, tbl
              else
                VFilter False, tbl
            | PossibleVals set -> if ValSet.mem v set then
                VTest(h,v), insert tbl h (ExactVal v)
              else
                VFilter False, tbl
            | AnyVal -> VTest(h,v), insert tbl h (ExactVal v)
            | NoVal -> failwith "NoVal is not legal value in this analysis"
        end
      | VSeq(p,q) ->
        let p', tbl' = remove_dead_matches' p tbl in
        let q', tbl'' = remove_dead_matches' q tbl' in
        VSeq(p', q'), tbl''
      | VPar(p,q) ->
        let p', tbl' = remove_dead_matches' p tbl in
        let q', tbl'' = remove_dead_matches' q tbl in
        VPar(p',q'), merge tbl' tbl''
      (* Overapproximation: any value is possible *)
      | VStar(p) -> 
        (* First we obtain the most precise value table by iterating p
           until we reach a fix point, then we analyze p with that
           table *)
        let rec compute_value_table tbl' = 
          let p', tbl'' = remove_dead_matches' p tbl' in
          let tbl''' = (merge tbl' tbl'') in
          if val_list_eq tbl' tbl''' then 
            p',tbl'''
          else compute_value_table tbl''' in
        let p', tbl' = compute_value_table tbl in
        VStar(p'), merge tbl' tbl
      (* Overapproximation: pretend both branches get executed (like union)*)
      | VChoice(p,q) ->
        let p', tbl' = remove_dead_matches' p tbl in
        let q', tbl'' = remove_dead_matches' q tbl in
        VChoice(p',q'), merge tbl' tbl''
      | _ -> p,tbl

  let remove_dead_matches p = 
    (* let () = Printf.printf "remove_dead_matches in: %s\n%!" (string_of_vpolicy p) in *)
    let ret = fst (remove_dead_matches' p []) in
    (* let () = Printf.printf "remove_dead_matches out: %s\n%!" (string_of_vpolicy ret) in *)
    ret

  type var_use = Used | Unused

  let var_join u u' =
    match u,u' with
      | Used, _ -> Used
      | _, Used -> Used
      | _, _ -> Unused

  let var_meet u u' =
    match u,u' with
      | Unused, _ -> Unused
      | _, Unused -> Unused
      | _, _ -> Used

  let rec remove_dead_mods' merge default p tbl =
    match p with
      | VSeq(p,q) -> let q', tbl' = remove_dead_mods' merge default q tbl in
                     let p', tbl'' = remove_dead_mods' merge default p tbl' in
                     VSeq(p',q'), tbl''
      | VPar(p,q) -> let p', tbl' = remove_dead_mods' merge default p tbl in
                     let q', tbl'' = remove_dead_mods' merge default q tbl in
                     VPar(p',q'), merge tbl' tbl''
      | VMod(Tag h, v) -> 
        begin
          match lookup' default tbl h with
            | Unused -> VFilter True, tbl
            | Used -> VMod(Tag h, v), insert tbl h Unused
        end
      | VTest(h,v) -> VTest(h,v), insert tbl h Used
      | VStar(p) -> let _, tbl' = remove_dead_mods' merge default p [] in
                    let tbl'' = merge tbl' tbl in
                    let p', _ = remove_dead_mods' merge default p tbl'' in
                    VStar(p'), tbl''
      | VChoice(p,q) -> let p', tbl' = remove_dead_mods' merge default p tbl in
                        let q', tbl'' = remove_dead_mods' merge default q tbl in
                        VChoice(p',q'), merge tbl' tbl''
      | _ -> p, tbl

  (* Remove unread mods: i.e (h <- v; ...; h <- v') where "..." does
     not read the value of h 
     Warning: Do NOT call this on arbitrary
     locations inside of a policy! This can only be called at the top
     level on the final policy. Instead, use remove_dead_mods_safe *)

  let remove_dead_mods p = 
    let ret = fst (remove_dead_mods' (merge' var_join Unused) Unused p []) in
    ret

  (* Safe to call on arbitrary policies *)
  let remove_dead_mods_safe p = fst (remove_dead_mods' (merge' var_join Used) Used p [])

  let merge_neg = merge' join AnyVal

  let string_of_var_val v = match v with
    | AnyVal -> "AnyVal"
    | NoVal -> "NoVal"
    | _ -> 
      failwith "Not yet implemented"

  (* Because we are using virtual headers, and no one else gets to use
     them, we know that the headers have no values until we initialize
     them. Thus, any match on a virtual header that is not preceded by a
     mod on the same header is "dead code", and can be eliminated
     (replaced by VFilter False) *)

  let rec elim_vtest' p tbl =
    match p with
      | VMod(Tag h, v) -> VMod(Tag h, v), insert tbl h AnyVal
      | VTest(h,v) -> 
        begin
          match lookup' NoVal tbl h with
            | AnyVal -> VTest(h,v) , tbl
            | NoVal -> VFilter False, tbl
            | _ -> failwith "This is not a legal value in this analysis"
        end
      | VSeq(p,q) ->
        let p', tbl' = elim_vtest' p tbl in
        let q', tbl'' = elim_vtest' q tbl' in
        VSeq(p', q'), tbl''
      | VPar(p,q) ->
        let p', tbl' = elim_vtest' p tbl in
        let q', tbl'' = elim_vtest' q tbl in
        VPar(p',q'), merge_neg tbl' tbl''
      (* Overapproximation *)
      | VStar(p) -> let _, tbl' = elim_vtest' p tbl in
                    VStar(p), tbl'
      (* Overapproximation: pretend both branches get executed (like union) *)
      | VChoice(p,q) ->
        let p', tbl' = elim_vtest' p tbl in
        let q', tbl'' = elim_vtest' q tbl in
        VChoice(p',q'), merge_neg tbl' tbl''
      | _ -> p,tbl

  let elim_vtest p = (* let () = Printf.printf "elim_vtest in: %s\n%!" (string_of_vpolicy p) in *)
                     let ret = fst (elim_vtest' p []) in
                     (* let () = Printf.printf "remove_dead_mods out: %s\n%!" (string_of_vpolicy ret) in *)
                     ret

  let rec optimize' p = 
    let simpl = simplify_vpol in
    let p' =  remove_dead_mods (remove_dead_matches (simpl p)) in
    if p' = p then p'
    else optimize' p'

  let rec optimize_safe p = 
    let simpl = simplify_vpol in
    let p' =  remove_dead_mods_safe (remove_dead_matches (simpl (vpol_to_linear_vpol p))) in
    if p' = p then p'
    else optimize_safe p'

  let rec atomic p = match p with
    | VTest _ -> true
    | VMod _ -> true
    | VFilter _ -> true
    | VLink _ -> true
    | _ -> false

  let rec seq_atomic p = match p with
    | VStar _ -> true
    | VSeq(p,q) -> seq_atomic p && seq_atomic q
    | _ -> atomic p

  let rec par_seq_atomic p = match p with
    | VPar(p,q) -> par_seq_atomic p && par_seq_atomic q
    | VStar _ -> true
    | _ -> seq_atomic p

  let rec choice_par_seq_atomic p = match p with
    | VStar _ -> true
    | VChoice(p,q) -> choice_par_seq_atomic p && choice_par_seq_atomic q
    | _ -> par_seq_atomic p

  (* Normal form: choice of unions of sequences *)
  let rec distribute_seq p =
    let ret = match p with
      | VSeq(VChoice(p,q), r) ->
        VChoice(distribute_seq (VSeq(p,r)), distribute_seq (VSeq(q,r)))
      | VSeq(p, VChoice(q,r)) ->
        VChoice(distribute_seq (VSeq(p,q)), distribute_seq (VSeq(p,r)))
      (* (p|q);r = p;r | q;r *)
      | VSeq(VPar(p,q), r) -> 
        let p_r = distribute_seq (VSeq(p,r)) in
        let q_r = distribute_seq (VSeq(q,r)) in
        begin
          match p_r,q_r with
            | VChoice _,_
            | _, VChoice _ -> (* let () = Printf.printf "VSeq_VPar p_r: %s\n\t q_r %s\n%!" (string_of_vpolicy p_r) (string_of_vpolicy q_r) in *)
              distribute_seq (VPar(p_r,q_r))
            | _,_ -> VPar(p_r, q_r)
        end
      (* p;(q|r) = p;q | p;r *)
      | VSeq(p, VPar(q,r)) -> 
        let p_q = distribute_seq (VSeq(p,q)) in
        let p_r = distribute_seq (VSeq(p,r)) in
        begin
          match p_q,p_r with
            | VChoice _,_ 
            | _, VChoice _ -> (* let () = Printf.printf "VSeq_VPar p_q: %s\n\t p_r %s\n%!" (string_of_vpolicy p_q) (string_of_vpolicy p_r) in *)
              distribute_seq (VPar(p_q,p_r))
            | _,_ -> VPar(p_q, p_r)
        end
      | VSeq(p,q) -> let p' = distribute_seq p in
                     let q' = distribute_seq q in
                     if seq_atomic p' & seq_atomic q' then
                       VSeq(p',q')
                     else
                       (* let () = Printf.printf "VSeq \tp': %s\n\t q' %s\n%!" (string_of_vpolicy p') (string_of_vpolicy q') in *)
                       distribute_seq(VSeq(p',q'))
      | VPar(VChoice(p,q), r) ->
        let p_r = distribute_seq (VPar(p,r)) in
        let q_r = distribute_seq (VPar(q,r)) in
        VChoice(p_r, q_r)
      | VPar(p, VChoice(q,r)) ->
        let p_q = distribute_seq (VPar(p,q)) in
        let p_r = distribute_seq (VPar(p,r)) in
        VChoice(p_q, p_r)
      | VPar(p,q) -> let p' = distribute_seq p in
                     let q' = distribute_seq q in 
                     if par_seq_atomic p' && par_seq_atomic q' then
                       VPar(p',q')
                     else
                       (* let () = Printf.printf "VPar p': %s\n\t q' %s\n%!" (string_of_vpolicy p') (string_of_vpolicy q') in *)
                       distribute_seq (VPar(p',q'))
      | VChoice(p,q) -> VChoice(distribute_seq p, distribute_seq q)
      | VStar(p) -> VStar(distribute_seq p)
      | _ -> p in
    optimize_safe ret

  let optimize p = 
    (* let renorm = vpol_to_linear_vpol in *)
    let simpl = simplify_vpol in
    let p' = (simpl (remove_dead_matches p)) in
    (* let () = Printf.printf "p': %s\n%!" (string_of_vpolicy p') in *)
    let p'' = (* distribute_seq *) p' in
    (* let () = Printf.printf "p'':\n%!" in     *)
    let p''' = simpl (remove_dead_mods (elim_vtest (optimize' (normalize_seq p'')))) in
    p'''

end  

  module ValSet = Set.Make (struct
    type t = header_val
    let compare = Pervasives.compare
  end)

(* Can't keep old members around, key must have pre-existing value *)
  let update_alist alist k v =
    List.map (fun (k',v') -> if k' = k then (k,v) else (k',v')) alist

let add_tag_value htbl h v = 
  if Hashtbl.mem htbl h then 
    Hashtbl.replace htbl h (ValSet.add v (Hashtbl.find htbl h))
  else 
    Hashtbl.add htbl h (ValSet.singleton v)
    
let rec collect_tags' p tags =
  match p with
    | VMod(Tag h,v) -> add_tag_value tags h v
    | VTest(h, v) -> add_tag_value tags h v
    | VPar(p,q) -> let () = collect_tags' p tags in
                   collect_tags' q tags
    | VSeq(p,q) -> let () = collect_tags' p tags in
                   collect_tags' q tags
    | VChoice(p,q) -> let () = collect_tags' p tags in
                      collect_tags' q tags
    | VStar(p) -> collect_tags' p tags
    | _ -> ()

(* let rec range n =  *)
(*   if n <= 0 then *)
(*     [VInt.Int16 0] *)
(*   else (VInt.Int16 n) :: (range (n - 1)) *)

let collect_tags p = let htbl = Hashtbl.create 20 in
                     let () = collect_tags' p htbl in
                     Hashtbl.fold (fun h valset acc -> (h, ValSet.elements valset) :: acc) htbl []

(* Assume we have a list of lists H, where the elements of H[i] are
   the values of h_i. Now, for each H[i][j], we want to compute a list
   representing each possible tuple containing value j in position
   i. We'll do this in several steps. First, represent this as
   [[[int]]] *)

let singletons h = List.fold_left (fun acc x -> [(h,x)]::acc) []

let rec prod h elems lst = 
  match elems with
    | []  -> []
    | elem :: elems -> (List.map (fun x -> (h, elem) :: x) lst) @ prod h elems lst

let rec make_tuples lst =
  match lst with
    | [] -> []
    | [(h,elems)] -> singletons h elems
    | (h,elems) :: lst -> prod h elems (make_tuples lst)

  (* Need a function that takes an old tag/value, and returns every
     matching new tag. *)
  (* Store the tag array as an alist, indexed by headers *)

  (* returns a list of alists, where each alist represents a tuple *)
let rec compute_matching_tuples tag_alist h v =
  make_tuples (update_alist tag_alist h [v])

  (* Takes an old value (h <- v), and returns a list of tuples (a,b)
     where b is a new value equivalent to (h=v) *)
let rec get_new_values tag_alist h v = 
  let old_vals = List.fold_left 
    (fun acc v' -> compute_matching_tuples tag_alist h v' @ acc) []
    (List.assoc h tag_alist) in
  List.map (fun old_val -> (old_val, update_alist old_val h v)) old_vals

let tuple_hashTbl = Hashtbl.create 100 
let tuple_cnter = ref 0
let tuple_to_int tpl =
  if Hashtbl.mem tuple_hashTbl tpl
  then VInt.Int16 (Hashtbl.find tuple_hashTbl tpl)
  else 
    let _ = incr tuple_cnter in
    let _ = Hashtbl.add tuple_hashTbl tpl !tuple_cnter in
    VInt.Int16 !tuple_cnter

(* We have to be careful here. The very first time we write the tag,
   we won't have any pre-existing values, so we can't match upon them *)
let rec convert_tag_to_hdr tag_alist h p written =
  match p with
    | VFilter pr -> Filter pr, written
    | VMod (Tag h', v') -> 
      let tuples = get_new_values tag_alist h' v' in
      if written then
        (List.fold_left (fun acc (old_v, new_v) ->
          Par(Seq(Filter(Test(Header h, tuple_to_int old_v)), Mod(Header h, tuple_to_int new_v)), acc)) (Filter False) tuples,
         written)
      else
        (* Just pick one legal tuple and use that. We always
           initialize other tags before matching on them, so the other
           values don't matter *)
        Mod(Header h, tuple_to_int (update_alist (fst (List.hd tuples)) h' v')), true
    | VTest(h',v') -> let tuples = compute_matching_tuples tag_alist h' v' in
                      List.fold_left (fun acc new_v -> Par(Filter(Test (Header h, tuple_to_int new_v)), acc)) (Filter False) tuples, written
    | VMod (Field h', v') -> Mod (h', v'), written
    | VSeq(p,q) -> let p', written = convert_tag_to_hdr tag_alist h p written in
                   let q', written = convert_tag_to_hdr tag_alist h q written in
                   Seq(p',q'), written
    | VPar(p,q) -> let p', written' = convert_tag_to_hdr tag_alist h p written in
                   let q', written'' = convert_tag_to_hdr tag_alist h q written in
                   Par(p',q'), written' || written''
    | VChoice(p,q) -> let p', written' = convert_tag_to_hdr tag_alist h p written in
                      let q', written'' = convert_tag_to_hdr tag_alist h q written in
                      Choice(p',q'), written' || written''
    | VStar(p) -> let p',written = convert_tag_to_hdr tag_alist h p written in
                  Star(p'), written
    | VLink(sw,pt,sw',pt') -> Link(sw,pt,sw',pt'), written

(* reduces policies by using identities (drop;p = drop, etc) *)
let rec simplify_pol p =
  match p with
    | Seq(Filter False, _) -> Filter False
    | Seq(_, Filter False) -> Filter False
    | Seq(Filter True, p) -> simplify_pol p
    | Seq(p, Filter True) -> simplify_pol p
    | Seq(p, q) -> let p' = simplify_pol p in
                    let q' = simplify_pol q in
                    if p' = p & q' = q then
                      Seq(p',q')
                    else
                      simplify_pol (Seq(p',q'))
    | Par(Filter False, p) -> simplify_pol p
    | Par(p, Filter False) -> simplify_pol p
    | Par(p, Par(q,r)) ->
      if p = q then simplify_pol (Par(q,r))
      else let p' = simplify_pol p in
           if p' = p then
             Par(p, simplify_pol (Par(q,r)))
           else
             simplify_pol (Par(p', Par(q,r)))
    | Par(p,q) -> 
      if p = q then p
      else
        let p' = simplify_pol p in
        let q' = simplify_pol q in
        if p' = p & q' = q then
          Par(p',q')
        else
          simplify_pol (Par(p',q'))
    | Choice(p, Choice(q,r)) ->
      if p = q then simplify_pol (Choice(q,r))
      else let p' = simplify_pol p in
           if p' = p then
             Choice(p, simplify_pol (Choice(q,r)))
           else
             simplify_pol (Choice(p', Choice(q,r)))
    | Choice(p,q) ->
      if p = q then p
      else
        let p' = simplify_pol p in
        let q' = simplify_pol q in
        if p' = p & q' = q
        then Choice(p',q')
        else simplify_pol (Choice(p',q'))
    | Star(Filter False) -> Filter True
    | Star(Filter True) -> Filter True
    | Star(p) -> 
      begin
        match simplify_pol p with
          | Filter False -> Filter True
          | Filter True -> Filter True
          | p -> Star p
      end
    | _ -> p

  (* Linearizes seqs (i.e. Seq(Seq(a,b),c) => Seq(a,Seq(b,c) *)
  let rec pol_to_linear_pol p = 
    match p with
      (* (p | q) | r = p (q | r) *)
      | Par (Par (p,q), r) ->
        pol_to_linear_pol (Par (p, Par(q,r)))
      | Par (p, q) ->
        let p' = pol_to_linear_pol p in
        let q' = pol_to_linear_pol q in
        if p' = p && q' = q then
          Par(p,q)
        else pol_to_linear_pol (Par(p',q'))
      (* (p;q);r = p;(q;r) *)
      | Seq (Seq (p,q), r) ->
        pol_to_linear_pol (Seq (p, Seq (q,r)))
      | Seq (p,q) ->
        let p' = pol_to_linear_pol p in
        let q' = pol_to_linear_pol q in
        if p' = p && q' = q then
          Seq(p,q)
        else pol_to_linear_pol (Seq(p',q'))
      | Choice(p,q) ->
        let p' = pol_to_linear_pol p in
        let q' = pol_to_linear_pol q in
        if p' = p && q' = q then
          Choice(p,q)
        else pol_to_linear_pol (Choice(p',q'))
      | Star p -> Star (pol_to_linear_pol p)
      | _ -> p

(* These assume linear pols *)
let rec separate_i p = match p with
  | Seq(Star _, p) -> Filter True
  | Seq(p,q) -> Seq(p, separate_i q)
  | _ -> p

let rec separate_t_from_pt p = match p with
  | Seq(p, Link (sw,pt,sw',pt')) -> Link (sw,pt,sw',pt')
  | Seq(p, Par (q,r)) -> Par (q,r)
  | Seq(p, q) -> separate_t_from_pt q
  | Link _ -> p
  | Filter True -> p
  | Filter False -> p
  | _ -> failwith (Printf.sprintf "unexpected t pol: %s" (string_of_policy p))

let rec separate_p_from_pt p = match p with
  | Seq(p, Link _) -> p
  | Seq(p, Par (q,r)) -> p
  | Seq(p,q) -> Seq(p, separate_t_from_pt q)
  | Filter True -> p

let rec separate_p p = match p with
  | Star(p) -> separate_p_from_pt p
  | Seq(Star(p), _) -> separate_p_from_pt p
  | Seq(p,q) -> separate_p q
  | _ -> Filter False

let rec separate_t p = match p with
  | Star(p) -> separate_t_from_pt p
  | Seq(Star(p), q) -> separate_t_from_pt p
  | Seq(p,q) -> separate_t q
  | _ -> Filter False

let rec separate_e p = match p with
  | Seq(Star _, p) -> p
  | Seq(p, q) -> separate_e q
  | _ -> Filter True

let separate_policy p = separate_i p, separate_p p, separate_t p, separate_e p

let strip_vlan = Mod (Header SDN_Types.Vlan,VInt.Int16 0xFFFF)

let vlan_none = Test (Header SDN_Types.Vlan,VInt.Int16 0xFFFF)

(* I'm unclear about the precise semantics of forwarding to hosts that
   aren't represented in the topology. To be safe, I'm stripping VLAN as soon as we forward out any port in e *)

let rec strip_vlans p = match p with
  | Mod(Header port, _) -> Seq(strip_vlan, p)
  | Seq(p,q) -> Seq(strip_vlans p, strip_vlans q)
  | Par(p,q) -> Par(strip_vlans p, strip_vlans q)
  | Choice(p,q) -> Choice(strip_vlans p, strip_vlans q)
  | Star(p) -> Star(strip_vlans p)
  | _ -> p

(* let dehop_policy p = *)
(*   let (i,s,t,e) = dehopify p in *)
(*   let p' = (VSeq(i, VSeq(VStar(VSeq(s,t)), e))) in *)
(*   fst (convert_tag_to_hdr (collect_tags p') SDN_Types.Vlan p' false) *)

let dehop_policy p =
  let (i,s,t,e) = dehopify (simplify_pol p) in
  let p' = Optimization.simplify_vpol (VSeq(i, VSeq(VStar(VSeq(s,t)), e))) in
  let () = Printf.printf "p': %s\n%!" (string_of_vpolicy p') in
  let p'' = Optimization.optimize p' in
  let () = Printf.printf "p'': %s\n%!" (string_of_vpolicy p'') in
  let p''' = pol_to_linear_pol (simplify_pol (fst (convert_tag_to_hdr (collect_tags p'') SDN_Types.Vlan p'' false))) in
  let () = Printf.printf "p''': %s\n%!" (string_of_policy p''') in
  let i',s',t',e' = separate_policy p''' in
  let () = Printf.printf "i': %s\n%!" (string_of_policy i') in
  let () = Printf.printf "e': %s\n%!" (string_of_policy e') in
  let () = Printf.printf "s': %s\n%!" (string_of_policy s') in
  let () = Printf.printf "t': %s\n%!" (string_of_policy t') in
  let i'',s'',t'',e'' = simplify_pol i', simplify_pol s', simplify_pol t', simplify_pol e' in
  (i'', s'', t'', Par (Seq (Filter vlan_none, e''), Seq(Filter (Neg vlan_none), Seq(strip_vlans e'', strip_vlan))))
