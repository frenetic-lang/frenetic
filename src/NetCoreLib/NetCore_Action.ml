open OpenFlow0x01
open OpenFlow0x01.Action
open Packet
open NetCore_Types

module type ACTION = sig
  type t

  type e

  val atoms : t -> e list

  val to_action : e -> t

  val drop : t

  val pass : t

  val apply_action : t -> lp -> lp list

  val par_action : t -> t -> t

  val seq_action : t -> t -> t

  val sequence_range : e -> ptrn -> ptrn

  val domain : e -> ptrn

  val is_equal : t -> t -> bool

 end

module Bool = struct
  type t = bool
  type e = bool

  let atoms b = [b]

  let drop = false

  let pass = true

  let to_action b = b

  let apply_atom b lp = if b then Some lp else None

  let apply_action action lp = 
    List.fold_right 
      (fun a acc -> match apply_atom a lp with 
          None -> acc
        | Some a' -> a'::acc) 
      (atoms action) []

  let par_action b1 b2 = b1 || b2

  let seq_action b1 b2 = b1 && b2

  let sequence_range b p = p

  let domain b = NetCore_Pattern.all

  let to_string b = if b then "true" else "false"

  let is_equal x y = x = y
end

module Output = struct
  open List
  open Packet
  open NetCore_Pattern
  open NetCore_Types

  type action = action_atom list

  type e = action_atom

  type t = e list

  let atoms act = act

  let to_action x = [x]

  let drop = []

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
    [ SwitchAction { unmodified with outDlVlan = Some (od,nw) } ]

  let updateSrcIP old new_ = 
    [ SwitchAction { unmodified with outNwSrc = Some (old, new_) } ]

  let updateDstIP old new_ = 
    [ SwitchAction { unmodified with outNwDst = Some (old, new_) } ]

  let updateSrcPort old new_ = 
    [ SwitchAction { unmodified with outTpSrc = Some (old, new_) } ]

  let updateDstPort old new_ = 
    [ SwitchAction { unmodified with outTpDst = Some (old, new_) } ]

  let maybe_modify nw modifier pk = match nw with
    | Some (a,v) ->
      modifier pk v
    | None ->
      pk

  let apply_output out (sw,pt,pkt) =
    let pt' = match out.outPort with
      | Here -> pt
      | pt -> pt in
    Some (sw, pt',
         (maybe_modify out.outDlSrc Packet.setDlSrc
         (maybe_modify out.outDlDst Packet.setDlDst
         (maybe_modify out.outDlVlan Packet.setDlVlan
         (maybe_modify out.outDlVlanPcp Packet.setDlVlanPcp
         (maybe_modify out.outNwSrc Packet.setNwSrc
         (maybe_modify out.outNwDst Packet.setNwDst
         (maybe_modify out.outNwTos Packet.setNwTos
         (maybe_modify out.outTpSrc Packet.setTpSrc
         (maybe_modify out.outTpDst Packet.setTpDst pkt))))))))))

  let rec apply_atom atom (sw,pt,pk) = match atom with
    | SwitchAction out -> 
      begin match apply_output out (sw,pt,pk) with
        | None -> []
        | Some lp -> [lp]
      end
    | ControllerAction f -> apply_action (f sw pt pk) (sw, pt, pk)
    | ControllerQuery _ -> []

  and apply_action act lp = 
    Frenetic_List.concat_map (fun a -> apply_atom a lp) act

  let par_action act1 act2 = act1 @ act2

  let par_actions = List.concat

  let seq_mod beq m1 m2 =
    match m1,m2 with
    | Some (a,b),Some(c,d) ->
      if beq b c then Some (Some (a, d)) else None
    | _,None ->
      Some m1
    | None,_ ->
      Some m2

  let seq_port pt1 pt2 = match (pt1, pt2) with
    | Here, _ -> pt2
    | _, Here -> pt1
    | _ -> pt2

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
             match apply_output out (sw, pt, pk) with
               | None -> drop
               | Some (sw', pt', pk') -> g sw' pt' pk'))
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

  let trans maybe_mod build_singleton set_wild pat =
    match maybe_mod with
    | Some (old, nw) ->
      if NetCore_Pattern.is_empty (NetCore_Pattern.inter (build_singleton nw) pat) then
        NetCore_Pattern.empty
      else
        set_wild pat
    | None -> pat

  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> NetCore_Pattern.all

  let restrict_port portMod pat2 = match portMod with
    | Here -> pat2
    | port ->
      if NetCore_Pattern.is_empty 
        (NetCore_Pattern.inter (NetCore_Pattern.inPort port) pat2) then
        NetCore_Pattern.empty
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
    let open NetCore_Pattern in
    restrict_port out.outPort
      (* TODO(arjun): Fill in the rest!!!! *)
      (trans out.outDlSrc dlSrc wildcardDlSrc
        (trans out.outDlDst dlDst wildcardDlDst
          (trans out.outDlVlan dlVlan wildcardDlVlan pat)))

  let sequence_range atom pat = match atom with
    | SwitchAction out        -> sequence_range_switch out pat
    | ControllerAction _      -> pat
    | ControllerQuery _   -> pat

  let domain atom = match atom with
    | SwitchAction out -> 
      fold_right
        NetCore_Pattern.inter
        [ sel NetCore_Pattern.dlSrc out.outDlSrc
        ; sel NetCore_Pattern.dlDst out.outDlDst
        ; sel NetCore_Pattern.dlVlan out.outDlVlan ]
        NetCore_Pattern.all
    | ControllerAction _      -> NetCore_Pattern.all
    | ControllerQuery _   -> NetCore_Pattern.all

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
    | Here -> [] (* Fishy, IMO. Shouldn't this be InPort? *)
    | All -> modify out @ (Output PseudoPort.AllPorts)
      :: unmodify out
    | Physical pt ->
      modify out @
        (( match inp with
         | Some pt' when (=) pt' pt ->
             Output PseudoPort.InPort
         | _ ->
           Output (PseudoPort.PhysicalPort pt)) ::
          (unmodify out))
   
  let atom_to_of inp atom = match atom with
    | SwitchAction out -> output_to_of inp out
    | ControllerAction _ -> [ Output (PseudoPort.Controller 65535) ]
    | ControllerQuery _ -> []

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

  let as_actionSequence inp act = 
    Frenetic_List.concat_map (atom_to_of inp) act

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
