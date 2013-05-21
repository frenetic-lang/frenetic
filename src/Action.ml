open Misc
open Packet
open OpenFlow0x01

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

module Bool = struct
  type t = bool
  type e = bool

  let atoms b = [b]

  let drop = false

  let pass = true

  let to_action b = b

  let apply_atom b ptpk = if b then Some ptpk else None

  let apply_action action ptpk = filter_map (fun a -> apply_atom a ptpk) (atoms action)

  let par_action b1 b2 = b1 || b2

  let seq_action b1 b2 = b1 && b2

  let restrict_range b p = p

  let domain b = Pattern.all

  let to_string b = if b then "true" else "false"

end

module Output = struct
  open Word
  open Misc
  open List
  open Packet
  open Pattern
  open Misc

  type 'a match_modify = ('a * 'a) option

  (** Note that OpenFlow does not allow the [dlType] and [nwProto]
      fields to be modified. *)
  type output =
    { outDlSrc : dlAddr match_modify;
      outDlDst : dlAddr match_modify;
      outDlVlan : dlVlan match_modify;
      outDlVlanPcp : dlVlanPcp match_modify;
      outNwSrc : nwAddr match_modify;
      outNwDst : nwAddr match_modify;
      outNwTos : nwTos match_modify;
      outTpSrc : tpPort match_modify;
      outTpDst : tpPort match_modify;
      outPort : Pattern.port option }

  (* JNF: does this belong in Misc? *)
  let string_of_output out =
    let string_of_pair f = Misc.string_of_pair f f in
    let reflections =
      [ ("DlSrc", string_of_option (string_of_pair dlAddr_to_string) out.outDlSrc)
      ; ("DlDst", string_of_option (string_of_pair dlAddr_to_string) out.outDlDst)
      ; ("DlVlan", string_of_option (string_of_pair dlVlan_to_string) out.outDlVlan)
      ; ("DlVlanPcp", string_of_option (string_of_pair dlVlanPcp_to_string) out.outDlVlanPcp)
      ; ("NwSrc", string_of_option (string_of_pair nwAddr_to_string) out.outNwSrc)
      ; ("NwDst", string_of_option (string_of_pair nwAddr_to_string) out.outNwDst)
      ; ("NwTos", string_of_option (string_of_pair nwTos_to_string) out.outNwTos)
      ; ("TpSrc", string_of_option (string_of_pair tpPort_to_string) out.outTpSrc)
      ; ("TpDst", string_of_option (string_of_pair tpPort_to_string) out.outTpDst)
      ; ("Fwd", string_of_option Pattern.string_of_port out.outPort) ] in
    let nonempty = List.filter (fun (f,v) -> v <> "") reflections in
    let rvs = List.map (fun (f,v) -> Printf.sprintf "%s %s" f v) nonempty in
    Printf.sprintf "{%s}" (String.concat ", " rvs)

  let to_string output_list =
    Printf.sprintf "[%s]"
      (String.concat ", " (List.map string_of_output output_list))

  type e = output

  type t = output list

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
      outPort = None }

  let pass = [unmodified]

  let forward pt =
    [ { unmodified with outPort = Some (Pattern.Physical pt) } ]

  let to_all = [ { unmodified with outPort = Some Pattern.All } ]

  let bucket n =
    [ { unmodified with outPort = Some (Pattern.Bucket n) } ]

  let updateDlSrc od nw =
    [ { unmodified with outDlSrc = Some (od, nw) } ]

  let updateDlDst od nw =
    [ { unmodified with outDlDst = Some (od, nw) } ]

  let updateDlVlan od nw =
    [ { unmodified with outDlVlan = Some (od,nw) } ]

  let par_action act1 act2 = act1 @ act2

  let seq_mod beq m1 m2 =
    match m1,m2 with
    | Some (a,b),Some(c,d) ->
      if beq b c then Some (Some (a, d)) else None
    | _,None ->
      Some m1
    | None,_ ->
      Some m2

  let seq_port pt1 pt2 =
    match pt1,pt2 with
    | None, _ -> pt2
    | _, None -> pt1
    | _ -> pt2

  let optword16beq w1 w2 =
    match w1,w2 with
    | Some w3,Some w4 ->
      Word16.eq_dec w3 w4
    | None,None ->
      true
    | _ ->
      false

  let seq_output out1 out2 = match
      (seq_mod Word48.eq_dec out1.outDlSrc out2.outDlSrc,
       seq_mod Word48.eq_dec out1.outDlDst out2.outDlDst,
       seq_mod optword16beq out1.outDlVlan out2.outDlVlan,
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

  let cross lst1 lst2 = concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1

  let seq_action act1 act2 =
    filter_map
      (fun (o1,o2) -> seq_output o1 o2)
      (cross act1 act2)

  let maybe_modify nw modifier pk = match nw with
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
    let pt' =
      match out.outPort with
      | Some pt' -> pt'
      | None -> pt in
    Some (pt',
         (maybe_modify out.outDlSrc Packet.setDlSrc
         (maybe_modify out.outDlDst Packet.setDlDst
         (maybe_modify out.outDlVlan Packet.setDlVlan
         (maybe_modify out.outDlVlanPcp Packet.setDlVlanPcp
         (maybe_modify out.outNwSrc Packet.setNwSrc
         (maybe_modify out.outNwDst Packet.setNwDst
         (maybe_modify out.outNwTos Packet.setNwTos
         (maybe_modify out.outTpSrc Packet.setTpSrc
         (maybe_modify out.outTpDst Packet.setTpDst pkt))))))))))

  let trans maybe_mod build_singleton set_wild pat =
    match maybe_mod with
    | Some (old, nw) ->
      if Pattern.is_empty (Pattern.inter (build_singleton nw) pat) then
        Pattern.empty
      else
        set_wild pat
    | None -> pat

  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> Pattern.all

  let restrict_port portMod pat2 = match portMod with
    | Some port ->
      if Pattern.is_empty (Pattern.inter (Pattern.inPort port) pat2) then
        Pattern.empty
      else
        Pattern.wildcardPort pat2
    | None -> pat2

  (* Restrict range: for each field f,
   *
   *    - if out updates f to v, and pat matches v, then replace v with
   *      wildcard in pat.
   *    - if out updates f to v, and pat does not match v, then replace
   *      v with None in pat.
   *    - if out does not update f, leave pat unchanged w.r.t. f.
   *)
  let restrict_range out pat =
    let open Pattern in
    restrict_port out.outPort
      (trans out.outDlSrc dlSrc wildcardDlSrc
        (trans out.outDlDst dlDst wildcardDlDst
          (trans out.outDlVlan dlVlan wildcardDlVlan pat)))

  let domain out =
    fold_right
      Pattern.inter
      [ sel Pattern.dlSrc out.outDlSrc
      ; sel Pattern.dlDst out.outDlDst
      ; sel Pattern.dlVlan out.outDlVlan ]
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
    | Some Pattern.All -> modify out @ (Output PseudoPort.AllPorts)
      :: unmodify out
    | Some (Pattern.Physical pt) ->
      modify out @
        (( match inp with
         | Some pt' when Word16.eq_dec pt' pt ->
             Output PseudoPort.InPort
         | _ ->
           Output (PseudoPort.PhysicalPort pt)) ::
          (unmodify out))
    | Some (Pattern.Bucket n) ->
      [ Output (PseudoPort.Controller Word16.max_value) ]
    | None ->
      []

  let as_actionSequence inp act =
    concat_map (output_to_of inp) act

  let apply_action act ptpk =
    filter_map (fun a -> apply_atom a ptpk) act
end
