open Misc
open NetworkPacket
open OpenFlow0x01Types

module type ACTION = sig 
  type t 
  
  type e 
  
  val atoms : t -> e list

  val to_action : e -> t
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (Pattern.port * packet) -> (Pattern.port * packet) option
  
  val apply_action : t -> (Pattern.port * packet) -> (Pattern.port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> Pattern.t -> Pattern.t
  
  val domain : e -> Pattern.t

  val to_string : t -> string
 end

module type CLASSIFIER = sig 
  module Action : ACTION

  type t = (Pattern.t * Action.t) list
  
  val scan : t -> Pattern.port -> packet -> Action.t
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : Action.t list -> Action.t

  val to_string : t -> string
end

module Bool = struct
  type t = bool
  type e = bool
  let atoms b = [b]
    
  let drop = false
  let pass = true

  let to_action b = b
    
  let apply_atom b ptpk = if b then Some ptpk else None
  
  let apply_action action ptpk =
    filter_map (fun a -> apply_atom a ptpk) (atoms action)
  
  let par_action b1 b2 =
    (||) b1 b2
  
  let seq_action b1 b2 =
    (&&) b1 b2
  
  let restrict_range b p = p
  
  let domain b = Pattern.all

  let to_string b = match b with
    | true -> "true"
    | false -> "false"

end

module Output = struct

  open Word
  open Misc
  open List
  open NetworkPacket
  open OpenFlow0x01Types
  open Pattern
  open Misc
  
  type 'a match_modify = ('a * 'a) option
  
  (** OpenFlow does not allow the [dlType] and [nwProto] fields to be 
      modified. *)
  type output = { outDlSrc : dlAddr match_modify;
                  outDlDst : dlAddr match_modify;
                  outDlVlan : dlVlan match_modify;
                  outDlVlanPcp : dlVlanPcp match_modify;
                  outNwSrc : nwAddr match_modify;
                  outNwDst : nwAddr match_modify;
                  outNwTos : nwTos match_modify;
                  outTpSrc : tpPort match_modify;
                  outTpDst : tpPort match_modify; 
                  outPort : Pattern.port }

  (* JNF: does this belong in Misc? *)
  let string_of_output out = 
    let opt_to_string to_string = function
      | Some v -> to_string v
      | None -> "" in 
    let reflections = 
      [ ("DlSrc", opt_to_string (string_of_pair dlAddr_to_string) out.outDlSrc)
      ; ("DlDst", opt_to_string (string_of_pair dlAddr_to_string) out.outDlDst)
      ; ("DlVlan", opt_to_string (string_of_pair dlVlan_to_string) out.outDlVlan)
      ; ("DlVlanPcp", opt_to_string (string_of_pair dlVlanPcp_to_string) out.outDlVlanPcp)
      ; ("NwSrc", opt_to_string (string_of_pair nwAddr_to_string) out.outNwSrc)
      ; ("NwDst", opt_to_string (string_of_pair nwAddr_to_string) out.outNwDst)
      ; ("NwTos", opt_to_string (string_of_pair nwTos_to_string) out.outNwTos)
      ; ("TpSrc", opt_to_string (string_of_pair tpPort_to_string) out.outTpSrc)
      ; ("TpDst", opt_to_string (string_of_pair tpPort_to_string) out.outTpDst)
      ; ("Fwd", Pattern.string_of_port out.outPort) ] in 
    let nonempty = List.filter (fun (f,v) -> v <> "") reflections in
    let rvs = List.map (fun (f,v) -> Printf.sprintf "%s %s" f v) nonempty in
    Printf.sprintf "{%s}"
      (String.concat ", " rvs)

  let to_string output_list =
    Printf.sprintf "[%s]" 
      (String.concat ", " (List.map string_of_output output_list)) 
  
  
  type e = output

  type t = output list
  
  let atoms act = act

  let to_action x = [x]
  
  let drop = []
  
  let id =
    { outDlSrc = None; 
      outDlDst = None; 
      outDlVlan = None; 
      outDlVlanPcp = None;
      outNwSrc = None; 
      outNwDst = None; 
      outNwTos = None; 
      outTpSrc = None; 
      outTpDst = None; 
      outPort = Pattern.Here } 
      
  let pass = [id]
    
  let forward pt =
    [ { id with outPort = Pattern.Physical pt } ]
  
  let bucket n =
    [ { id with outPort = Pattern.Bucket n } ] 
  
  let updateDlSrc od nw =
    [ { id with outDlSrc = Some (od, nw) } ]

  let updateDlDst od nw =
    [ { id with outDlDst = Some (od, nw) } ]
  
  let updateDlVlan od nw = 
    [ { id with outDlVlan = Some (od,nw) } ]

  let par_action act1 act2 = 
    act1 @ act2
  
  let seq_mod beq m1 m2 =
    match m1 with
    | Some (a,b) ->
      (match m2 with
       | Some (c,d) ->
         if beq b c then Some (Some (a, d)) else None
       | None -> Some m1)
    | None -> Some m2
  
  let seq_port pt1 pt2 =
    match pt1,pt2 with
    | Pattern.Here,_ -> pt2
    | _,Pattern.Here -> pt1
    | _ -> pt2
  
  let optword16beq w1 w2 =
    match w1 with
    | Some w3 ->
      (match w2 with
       | Some w4 -> Word16.eq_dec w3 w4
       | None -> false)
    | None ->
      (match w2 with
       | Some y -> false
       | None -> true)
  
  let seq_output out1 out2 =
    let beq_vlan v1 v2 = 
      match (v1, v2) with
      | Some w1, Some w2 -> Word16.eq_dec w1 w2
      | None, None -> true
      | _,_ -> false in 
    match 
      (seq_mod Word48.eq_dec out1.outDlSrc out2.outDlSrc,
       seq_mod Word48.eq_dec out1.outDlDst out2.outDlDst,
       seq_mod beq_vlan out1.outDlVlan out2.outDlVlan,
       seq_mod Word8.eq_dec out1.outDlVlanPcp out2.outDlVlanPcp,
       seq_mod Word32.eq_dec out1.outNwSrc out2.outNwSrc, 
       seq_mod Word32.eq_dec out1.outNwDst out2.outNwDst, 
       seq_mod Word8.eq_dec out1.outNwTos out2.outNwTos, 
       seq_mod Word16.eq_dec out1.outTpSrc out2.outTpSrc,
       seq_mod Word16.eq_dec out1.outTpDst out2.outTpDst) with 
      | ( Some dlSrc, 
          Some dlDst, 
          Some dlVlan,
          Some dlVlanPcp,
          Some nwSrc,
          Some nwDst,
          Some nwTos,
          Some tpSrc,
          Some tpDst ) -> 
        Some { outDlSrc = dlSrc; 
               outDlDst = dlDst;
               outDlVlan = dlVlan; 
               outDlVlanPcp = dlVlanPcp; 
               outNwSrc = nwSrc; 
               outNwDst = nwDst; 
               outNwTos = nwTos; 
               outTpSrc = tpSrc;
               outTpDst = tpDst; 
               outPort = seq_port out1.outPort out2.outPort }
      | _ -> None
  
  let cross lst1 lst2 =
    concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1
  
  let seq_action act1 act2 =
    filter_map 
      (fun (o1,o2) -> seq_output o1 o2)
      (cross act1 act2)
  
  let maybe_modify nw modifier pk =
    match nw with
    | Some (a,v) -> 
      modifier pk v
    | None -> 
      pk
  
  (* JNF: seriously? *)
  let withVlanNone = function
  | Some (Some od,Some nw) ->
    Some (od, nw)
  | Some (Some od,None) ->
    Some (od, None)
  | Some (None,Some nw) -> 
    Some (None, nw)
  | Some (None, None) -> 
    Some (None, None)
  | None -> None

  let apply_atom out (pt,pkt) = 
    Some (out.outPort,
          (maybe_modify out.outDlSrc NetworkPacket.setDlSrc
             (maybe_modify out.outDlDst NetworkPacket.setDlDst
                (maybe_modify out.outDlVlan NetworkPacket.setDlVlan
                   (maybe_modify out.outDlVlanPcp NetworkPacket.setDlVlanPcp
                      (maybe_modify out.outNwSrc NetworkPacket.setNwSrc
                         (maybe_modify out.outNwDst NetworkPacket.setNwDst
                            (maybe_modify out.outNwTos NetworkPacket.setNwTos
                               (maybe_modify out.outTpSrc NetworkPacket.setTpSrc
                                  (maybe_modify out.outTpDst NetworkPacket.setTpDst pkt))))))))))
  
  let trans x f pat = match x with 
  | Some (a,nw) -> f nw pat
  | None -> pat
  
  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> Pattern.all
  
  let restrict_port port1 pat2 = 
    if Pattern.is_empty (Pattern.inter (Pattern.inPort port1) pat2) then
      Pattern.empty
    else
      Pattern.wildcardPort pat2

  let restrict_range out pat =
    restrict_port out.outPort
      (trans out.outDlSrc Pattern.setDlSrc 
         (trans out.outDlDst Pattern.setDlDst pat))
  
  let domain out =
    fold_right 
      Pattern.inter 
      [ sel Pattern.dlSrc out.outDlSrc
      ; sel Pattern.dlDst out.outDlDst ]
      Pattern.all  
      
  let set upd mk lst = match upd with 
    | Some (_,nw) ->  
      (mk nw) :: lst
    | None -> 
      lst
  
  let unset upd mk lst = match upd with 
    | Some (od,_) -> 
      (mk od) :: lst
    | None -> 
      lst
  
  let setDlVlan' = function
  | Some n -> 
    SetDlVlan n
  | None -> 
    StripVlan
  
  let modify out =
    set out.outDlSrc (fun x -> SetDlSrc x)
      (set out.outDlDst (fun x -> SetDlDst x)
        (set out.outDlVlan (fun x -> SetDlVlan x)
          (set out.outDlVlanPcp (fun x -> SetDlVlanPcp x)
            (set out.outNwSrc (fun x -> SetNwSrc x)
              (set out.outNwDst (fun x -> SetNwDst x)
                (set out.outNwTos (fun x -> SetNwTos x)
                  (set out.outTpSrc (fun x -> SetTpSrc x)
                    (set out.outTpDst (fun x -> SetTpDst x) []))))))))
  
  let unmodify out =
    unset out.outDlSrc (fun x -> SetDlSrc x)
      (unset out.outDlDst (fun x -> SetDlDst x)
        (unset out.outDlVlan (fun x -> SetDlVlan x)
          (unset out.outDlVlanPcp (fun x -> SetDlVlanPcp x)
            (unset out.outNwSrc (fun x -> SetNwSrc x)
              (unset out.outNwDst (fun x -> SetNwDst x)
                (unset out.outNwTos (fun x -> SetNwTos x)
                  (unset out.outTpSrc (fun x -> SetTpSrc x)
                    (unset out.outTpDst (fun x -> SetTpDst x) []))))))))
  
  let output_to_of inp out = match out.outPort with
    | Pattern.Physical pt ->
      modify out @ 
        (( match inp with 
         | Some pt' when Word16.eq_dec pt' pt -> 
             Output InPort
         | _ -> 
           Output (PhysicalPort pt)) ::
          (unmodify out))
    | Pattern.Here -> 
      []
    | Pattern.Bucket n -> 
      [ Output (Controller Word16.max_value) ]
  
  let as_actionSequence inp act =
    concat_map (output_to_of inp) act
  
  let apply_action act ptpk =
    filter_map (fun a -> apply_atom a ptpk) act
end



module type MAKE  = functor (Action : ACTION) -> 
  sig include CLASSIFIER end
  with module Action = Action

module Make : MAKE = 
 functor (Action:ACTION) ->
 struct 
  module Action = Action
    
  type action = Action.t
  
  type t = (Pattern.t * action) list
    
  let rec scan' default classifier pt pk = match classifier with
    | [] -> default
    | p :: rest ->
      let (pat, a) = p in
      if Pattern.match_packet pt pk pat then a else scan' default rest pt pk
  
  let scan =
    scan' Action.drop
  
  let rec elim_shadowed_helper prefix = function
  | [] -> prefix
  | p :: cf' ->
    let (pat, act) = p in
    if List.exists (fun entry ->
         let (pat', act0) = entry in pat = pat') prefix
    then elim_shadowed_helper prefix cf'
    else elim_shadowed_helper (prefix @ ((pat, act) :: [])) cf'
  
  let elim_shadowed cf =
    elim_shadowed_helper [] cf
  
  let rec strip_empty_rules = function
  | [] -> []
  | p :: cf0 ->
    let (pat, acts) = p in
    if Pattern.is_empty pat
    then strip_empty_rules cf0
    else (pat, acts) :: (strip_empty_rules cf0)
  
  let opt tbl =
    elim_shadowed (strip_empty_rules tbl)
  
  let inter_entry cl = function
  | (pat, act) ->
    List.fold_right (fun v' acc ->
      let (pat', act') = v' in
      ((Pattern.inter pat pat'), (Action.par_action act act')) :: acc) [] cl
  
  let inter_no_opt cl1 cl2 =
    List.fold_right (fun v acc -> (inter_entry cl2 v) @ acc) [] cl1
  
  let union_no_opt cl1 cl2 =
    (inter_no_opt cl1 cl2) @ cl1 @ cl2
  
  let rec par_actions = function
  | [] -> Action.drop
  | act :: lst' -> Action.par_action act (par_actions lst')
  
  let seq tbl1 tbl2 pt pk =
    Action.seq_action (scan tbl1 pt pk)
      (par_actions
        (List.map (fun ptpk -> let (pt0, pk0) = ptpk in scan tbl2 pt0 pk0)
          (Action.apply_action (scan tbl1 pt pk) (pt, pk))))
  
  let union tbl1 tbl2 =
    opt (union_no_opt tbl1 tbl2)
  
  let inter tbl1 tbl2 =
    opt (inter_no_opt tbl1 tbl2)
    
  let rec unions = function
  | [] -> []
  | tbl :: lst' -> union_no_opt tbl (unions lst')
  
  (* [p1] is the domain restriction from the first table in the sequence.
     [a1] is ??? 
     [atom] is the first action that was applied.
  *)
  let rec pick p1 atom = function
  | [] -> []
  | (p2,a) :: tbl2' ->
    Format.printf "doing %s %s %s\n%!"
      (Pattern.to_string p1)
      (Pattern.to_string (Action.domain atom))
      (Pattern.to_string (Action.restrict_range atom p2));
    (Pattern.inter
       p1 
       (Pattern.inter 
          (Action.domain atom) 
          (Action.restrict_range atom p2)),
     Action.seq_action (Action.to_action atom) a) :: (pick p1 atom tbl2')
  
  let rec sequence_no_opt tbl1 tbl2 =
    match tbl1 with
    | [] -> []
    | (p,a) :: tbl1' ->
      match Action.atoms a with
        | [] -> (p, Action.drop) :: (sequence_no_opt tbl1' tbl2)
        | lst ->
          (unions (List.map (fun atom -> pick p atom tbl2) lst))
          @ (sequence_no_opt tbl1' tbl2)
  
  let sequence tbl1 tbl2 =
    opt (sequence_no_opt tbl1 tbl2)

  let to_string tbl = 
    let buf = Buffer.create 100 in
    List.iter
      (fun (pat,act) ->
        Buffer.add_string buf (Pattern.to_string pat);
        Buffer.add_string buf " => ";
        Buffer.add_string buf (Action.to_string act);
        Buffer.add_string buf "\n")
      tbl;
    Buffer.contents buf

 end

