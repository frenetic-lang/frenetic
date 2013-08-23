open Packet
open NetCore_Types

module type ACTION = sig
  type t

  type e

  val atoms : t -> e list

  val to_action : e -> t

  val drop : t

  val pass : t

  (* MJR: Should this change to lp list list now that we have non-det actions? *)
  val apply_action : t -> lp -> lp list

  val par_action : t -> t -> t

  val seq_action : t -> t -> t

  val alt_action : t -> t -> t

  val sequence_range : e -> ptrn -> ptrn

  val domain : e -> ptrn

  val is_equal : t -> t -> bool

  val atom_is_equal : e -> e -> bool

  val string_of_action : t -> string

 end

module type COMPILER_ACTION0x01 =
  sig
    include ACTION
      with type e = action_atom
    val from_nc_action : action -> t
    (* val as_actionSequence : portId option -> t -> OpenFlow0x01.Action.sequence *)
    val queries : t -> e list
  end

module Output =
struct
  open List
  open Packet
  open NetCore_Pattern
  open NetCore_Types

  type action = action_atom list

  type e = action_atom

  type t = e list

  let string_of_action = NetCore_Pretty.string_of_action

  let atoms act = act

  let to_action x = [x]
    
  let from_nc_action x = x

  let drop = []

  let par_action act1 act2 = act1 @ act2

  let par_actions = List.concat

  let unmodified =
    { outDlSrc = None;
      outDlDst = None;
      outDlVlan = None;
      outDlVlanPcp = None;
      outNwSrc = None;
      outNwDst = None;
      outNwTos = None;
      outTpSrc = None;
      outTpDst = None;
      outPort = Here }

  let pass = [SwitchAction unmodified]

  let forward pt =
    [ SwitchAction { unmodified with outPort = Physical pt } ]

  let to_all = [ SwitchAction { unmodified with outPort = All } ]

  let query time f =
    [ ControllerQuery (time, f) ]

  let controller handler =
    [ ControllerAction handler ]

  let updateDlSrc od nw =
    [ SwitchAction { unmodified with outDlSrc = Some (od, nw) } ]

  let updateDlDst od nw =
    [ SwitchAction { unmodified with outDlDst = Some (od, nw) } ]

  let updateDlVlan od nw =
    [ SwitchAction { unmodified with outDlVlan = Some (od, nw) } ]

  let updateDlVlanPcp od nw =
    [ SwitchAction { unmodified with outDlVlanPcp = Some (od, nw) } ]

  let updateSrcIP old new_ = 
    [ SwitchAction { unmodified with outNwSrc = Some (old, new_) } ]

  let updateDstIP old new_ = 
    [ SwitchAction { unmodified with outNwDst = Some (old, new_) } ]

  let updateTosIP old new_ = 
    [ SwitchAction { unmodified with outNwTos = Some (old, new_) } ]

  let updateSrcPort old new_ = 
    [ SwitchAction { unmodified with outTpSrc = Some (old, new_) } ]

  let updateDstPort old new_ = 
    [ SwitchAction { unmodified with outTpDst = Some (old, new_) } ]

  let updatePort new_ =
    [ SwitchAction { unmodified with outPort = new_ } ]

  let maybe_transform old new_ transformer =
    if old <> new_ then
      transformer old new_
    else
      drop

  let maybe_transform_port old new_ transformer = 
    if old <> new_ then
      transformer new_
    else
      drop

  (* Return an action [act] such that [eval_action p1 act] = [p2]. *)
  let make_transformer v1 v2 =
    let Pkt (_, pt1, p1, _) = v1 in
    let Pkt (_, pt2, p2, _) = v2 in
    let eth_actions =
      [ maybe_transform_port pt1 pt2 updatePort
      ; maybe_transform p1.dlSrc p2.dlSrc updateDlSrc
      ; maybe_transform p1.dlDst p2.dlDst updateDlDst
      ; maybe_transform p1.dlVlan p2.dlVlan updateDlVlan
      ; maybe_transform p1.dlVlanPcp p2.dlVlanPcp updateDlVlanPcp ] in
    let ip_actions = match (p1.nw, p2.nw) with 
      | Ip ip1, Ip ip2 ->
        let open Ip in
        let ip_acts =
          [ maybe_transform ip1.src ip2.src updateSrcIP
          ; maybe_transform ip1.dst ip2.dst updateDstIP
          ; maybe_transform ip1.tos ip2.tos updateTosIP ] in
        let tcp_acts = begin match (ip1.tp, ip2.tp) with
          | Tcp tcp1, Tcp tcp2 ->
            [ maybe_transform tcp1.Tcp.src tcp2.Tcp.src updateSrcPort
            ; maybe_transform tcp1.Tcp.dst tcp2.Tcp.dst updateDstPort ]
          | Icmp _, Icmp _
          | Unparsable _, Unparsable _ 
          | _, _ -> []
          end in
        ip_acts @ tcp_acts
      | Arp _, Arp _
      | Unparsable _, Unparsable _
        (* TODO(cole) warn/fail if these values differ? *)
      | _, _ ->
        (* TODO(cole) warn/fail that the policy somehow changed the frame 
         * type? *)
        []
      in
    List.fold_left par_action drop (eth_actions @ ip_actions)


  let maybe_modify nw modifier pk = match nw with
    | Some (a,v) ->
      modifier pk v
    | None ->
      pk

  let sel f = function
  | Some p -> let (old, _) = p in f old
  | None -> all

  let domain atom = match atom with
    | SwitchAction out -> 
      fold_right
        NetCore_Pattern.inter
        [ sel dlSrc out.outDlSrc
        ; sel dlDst out.outDlDst
        ; sel dlVlan out.outDlVlan
        ; sel dlVlanPcp out.outDlVlanPcp
        ; sel ipSrc out.outNwSrc
        ; sel ipDst out.outNwDst
        ; sel ipTos out.outNwTos
        ; sel tcpSrcPort out.outTpSrc
        ; sel tcpDstPort out.outTpDst ]
        all
    | ControllerAction _      -> all
    | ControllerQuery _   -> all

  let apply_output out (sw,pt,pkt) =
    let dom = domain (SwitchAction out) in
    if NetCore_Pattern.match_packet pt pkt dom then
      let pt' = match out.outPort with
        | Here -> pt
        | pt -> pt in
      Some
        (sw, pt',
        (maybe_modify out.outDlSrc Packet.setDlSrc
        (maybe_modify out.outDlDst Packet.setDlDst
        (maybe_modify out.outDlVlan Packet.setDlVlan
        (maybe_modify out.outDlVlanPcp Packet.setDlVlanPcp
        (maybe_modify out.outNwSrc Packet.setNwSrc
        (maybe_modify out.outNwDst Packet.setNwDst
        (maybe_modify out.outNwTos Packet.setNwTos
        (maybe_modify out.outTpSrc Packet.setTpSrc
        (maybe_modify out.outTpDst Packet.setTpDst pkt))))))))))
    else
      None

  let rec apply_atom atom (sw,pt,pk) = match atom with
    | SwitchAction out -> 
      begin match apply_output out (sw,pt,pk) with
        | Some v -> [v]
        | None -> []
      end
    | ControllerAction f -> apply_action (f sw pt pk) (sw, pt, pk)
    | ControllerQuery _ -> []

  and apply_action act lp = 
    Frenetic_List.concat_map (fun a -> apply_atom a lp) act

  let seq_port pt1 pt2 = match (pt1, pt2) with
    | Here, _ -> pt2
    | _, Here -> pt1
    | _ -> pt2

  let seq_mod beq m1 m2 =
    match m1,m2 with
    | Some (a,b),Some(c,d) ->
      if beq b c then Some (Some (a, d)) else None
    | _,None ->
      Some m1
    | None,_ ->
      Some m2

  let seq_output out1 out2 = match
      (seq_mod (=) out1.outDlSrc out2.outDlSrc,
       seq_mod (=) out1.outDlDst out2.outDlDst,
       seq_mod (=) out1.outDlVlan out2.outDlVlan,
       seq_mod (=) out1.outDlVlanPcp out2.outDlVlanPcp,
       seq_mod (=) out1.outNwSrc out2.outNwSrc,
       seq_mod (=) out1.outNwDst out2.outNwDst,
       seq_mod (=) out1.outNwTos out2.outNwTos,
       seq_mod (=) out1.outTpSrc out2.outTpSrc,
       seq_mod (=) out1.outTpDst out2.outTpDst) with
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
    Frenetic_List.concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1

  let rec seq_action_atom atom1 atom2 = match (atom1, atom2) with
    | SwitchAction out1, SwitchAction out2 -> 
      begin match seq_output out1 out2 with
        | Some out3 -> Some (SwitchAction out3)
        | None -> None
      end
    | ControllerAction f, ControllerAction g ->
      Some
        (ControllerAction 
           (fun sw pt pk ->
             (* 1st action produces new packets *) 
             let lps = apply_action (f sw pt pk) (sw, pt, pk) in 
             (* 2nd action is applied to the new packets, to get joint action *)
             par_actions (List.map (fun (sw', pt', pk') -> g sw' pt' pk') lps)))
    | SwitchAction out, ControllerAction g ->
      Some 
        (ControllerAction
           (fun sw pt pk ->
             begin match apply_output out (sw, pt, pk) with
             | Some (sw', pt', pk') -> g sw' pt' pk'
             | None -> []
             end))
    | ControllerAction f, SwitchAction out ->
      Some
        (ControllerAction
           (fun sw pt pk ->
             let atoms1 = f sw pt pk in
             Frenetic_List.filter_none
               (List.map
                  (fun at1 -> seq_action_atom at1 (SwitchAction out))
                  atoms1)))
    | ControllerQuery   _, _ ->
      (* Queries are functionally equivalent to drop.  But they count as a side
       * effect first. *)
      Some atom1
    | _, ControllerQuery   _ ->
      Some atom2

  let seq_action act1 act2 =
    Frenetic_List.filter_map
      (fun (o1,o2) -> seq_action_atom o1 o2)
      (cross act1 act2)

  let alt_action act1 act2 =
    failwith "NYI: alt_action"

  let trans maybe_mod build_singleton set_wild pat =
    match maybe_mod with
    | Some (old, nw) ->
      if NetCore_Pattern.is_empty (NetCore_Pattern.inter (build_singleton nw) pat) then
        empty
      else
        set_wild pat
    | None -> pat

  let restrict_port portMod pat2 = match portMod with
    | Here -> pat2
    | port ->
      if NetCore_Pattern.is_empty 
        (NetCore_Pattern.inter (inPort port) pat2) then
        empty
      else
        NetCore_Pattern.wildcardPort pat2

  (* Restrict range: for each field f,
   *
   *    - if out updates f to v, and pat matches v, then replace v with
   *      wildcard in pat.
   *    - if out updates f to v, and pat does not match v, then replace
   *      v with None in pat.
   *    - if out does not update f, leave pat unchanged w.r.t. f.
   *)
  let sequence_range_switch out pat =
    let pat = restrict_port out.outPort pat in
    let pat = trans out.outDlSrc dlSrc wildcardDlSrc pat in
    let pat = trans out.outDlDst dlDst wildcardDlDst pat in 
    let pat = trans out.outDlVlan dlVlan wildcardDlVlan pat in
    let pat = trans out.outDlVlanPcp dlVlanPcp wildcardDlVlanPcp pat in
    let pat = trans out.outNwSrc
      (fun v -> { all with ptrnNwSrc = WildcardExact v })
      wildcardNwSrc pat in
    let pat = trans out.outNwDst
      (fun v -> { all with ptrnNwDst = WildcardExact v })
      wildcardNwDst pat in
    let pat = trans out.outNwTos 
      (fun v -> { all with ptrnNwTos = WildcardExact v })
      wildcardNwTos pat in
    let pat = trans out.outTpSrc 
      (fun v -> { all with ptrnTpSrc = WildcardExact v })
      wildcardTpSrc pat in
    let pat = trans out.outTpDst
      (fun v -> { all with ptrnTpDst = WildcardExact v })
      wildcardTpDst pat in
    pat

  let sequence_range atom pat = match atom with
    | SwitchAction out        -> sequence_range_switch out pat
    | ControllerAction _      -> pat
    | ControllerQuery _   -> pat

  let apply_controller action (sw, pt, pk) =
    let f atom acc = match atom with
      | SwitchAction _ -> acc
      | ControllerAction f -> par_action (f sw pt pk) acc
      (* TODO(cole): don't ignore packets sent to the controller? *)
      | ControllerQuery _ -> acc
      in
    List.fold_right f action drop

  let switch_part action = 
    let f atom = match atom with
      | SwitchAction _ -> true
      | ControllerAction _ -> false
      | ControllerQuery _ -> false
      in
    List.filter f action


  let queries action =
    let f atom = match atom with
      | SwitchAction _ -> false
      | ControllerAction _ -> false
      | ControllerQuery _ -> true
      in
    List.filter f action

  let atom_is_equal x y = match x, y with
    | SwitchAction out1, SwitchAction out2 -> out1 = out2
    | ControllerAction f, ControllerAction g -> f == g (* functional values *)
    | ControllerQuery (time, f), ControllerQuery (time', f') -> 
      time == time' && f == f'
    | _ -> false

  (* TODO(arjun): What if they are permutations? *)
  let rec is_equal xs ys = match xs, ys with
    | [], [] -> true
    | x :: xs', y :: ys' ->
      atom_is_equal x y && is_equal xs' ys'
    | _ -> false
end

(* Action for OF >= 1.1 group actions. Intended interpretations:
   FF - [[a];[b]] = do action a unless it fails, else do action b
   roundrobin - [[a];[b]] = do action a, next time do action b. Repeat.
*)

module Group = struct
  open NetCore_Types

  type t = action list
  type e = action_atom

  let atoms a = List.flatten a

  let drop = []

  let pass = 
    [ Output.pass ]

  let forward pt = 
    [ Output.forward pt ]

  let to_all = 
    [ Output.to_all ]

  let query time f = 
    [ Output.query time f ]

  let controller handler = 
    [ Output.controller handler ]

  let updateDlSrc od nw =
    [ Output.updateDlSrc od nw ]

  let updateDlDst od nw =
    [ Output.updateDlDst od nw ]

  let updateDlVlan od nw =
    [ Output.updateDlVlan od nw ]

  let updateDlVlanPcp od nw =
    [ Output.updateDlVlanPcp od nw ]

  let updateSrcIP old new_ = 
    [ Output.updateSrcIP old new_ ]

  let updateDstIP old new_ = 
    [ Output.updateDstIP old new_ ]

  let updateTosIP old new_ = 
    [ Output.updateTosIP old new_ ]

  let updateSrcPort old new_ = 
    [ Output.updateSrcPort old new_ ]

  let updateDstPort old new_ = 
    [ Output.updateDstPort old new_ ]

  let updatePort new_ =
    [ Output.updatePort new_ ]

  let to_action a = [[a]]

  let from_nc_action x = [x]

  let make_transformer v1 v2 = [ Output.make_transformer v1 v2 ]

  let switch_part action = 
    let f atom = match atom with
      | SwitchAction _ -> true
      | ControllerAction _ -> false
      | ControllerQuery _ -> false
      in
    List.filter f action

  let apply_atom = Output.apply_atom

  (* Only uses the first action in the group *)
  let apply_action actions lp = match actions with
    | [] -> []
    | act :: actions -> 
      Frenetic_List.concat_map (fun a -> apply_atom a lp) act

  (* let rec prod lst1 lst2 = match lst1 with *)
  (*   | [] -> [] *)
  (*   | a :: lst1 -> (List.map (List.append x) lst2) @ prod lst1 lst2 *)

  let par_action b1 b2 = List.map (fun (a,b) -> a @ b) (Output.cross b1 b2)

  let seq_action b1 b2 = List.map (fun (a,b) -> Output.seq_action a b) (Output.cross b1 b2)

  let alt_action b1 b2 = b1 @ b2

  let sequence_range = Output.sequence_range

  let apply_controller action (sw, pt, pk) =
    let f atom acc = match atom with
      | SwitchAction _ -> acc
      | ControllerAction f -> par_action [(f sw pt pk)] acc
      (* TODO(cole): don't ignore packets sent to the controller? *)
      | ControllerQuery _ -> acc
      in
    List.fold_right f action drop

  (* let as_actionSequence inp acts = match acts with *)
  (*   | [] -> [] *)
  (*   | act :: acts -> Output.as_actionSequence inp act *)

  let queries x = atoms (List.map Output.queries x)

  let domain = Output.domain

  let is_equal x y = 
    try List.for_all2 Output.is_equal x y
    with Invalid_argument _ -> false

  let atom_is_equal = Output.atom_is_equal

  let string_of_action t =
        "[" ^ (String.concat "; " (List.map Output.string_of_action t)) ^ "]"

end
