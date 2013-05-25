open OpenFlow0x01
open OpenFlow0x01.Action
open Packet

let concat_map f lst =
  List.fold_right (fun a bs -> List.append (f a) bs) lst []
    
let rec filter_map f xs = match xs with
  | [] -> []
  | x :: xs' -> match f x with
    | Some y -> y :: (filter_map f xs')
    | None -> filter_map f xs'

module type ACTION = sig
  type t

  type e

  val atoms : t -> e list

  val to_action : e -> t

  val drop : t

  val pass : t

  val apply_atom : e -> (NetCore_Pattern.port * packet) -> (NetCore_Pattern.port * packet) option

  val apply_action : t -> (NetCore_Pattern.port * packet) -> (NetCore_Pattern.port * packet) list

  val par_action : t -> t -> t

  val seq_action : t -> t -> t

  val restrict_range : e -> NetCore_Pattern.t -> NetCore_Pattern.t

  val domain : e -> NetCore_Pattern.t

  val to_string : t -> string

  val is_equal : t -> t -> bool

 end

module Bool = struct
  type t = bool
  type e = bool

  let atoms b = [b]

  let drop = false

  let pass = true

  let to_action b = b

  let apply_atom b ptpk = if b then Some ptpk else None

  let apply_action action ptpk = List.fold_right (fun a acc -> match apply_atom a ptpk with None -> acc | Some a' -> a'::acc) (atoms action) []

  let par_action b1 b2 = b1 || b2

  let seq_action b1 b2 = b1 && b2

  let restrict_range b p = p

  let domain b = NetCore_Pattern.all

  let to_string b = if b then "true" else "false"

  let is_equal x y = x = y
end

module Output = struct
  open List
  open Packet
  open NetCore_Pattern

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
      outPort : NetCore_Pattern.port option }

  let match_modify_to_string
      (pr : 'a -> string) (lbl : string) (v : 'a match_modify) : string option =
    match v with
      | None -> None
      | Some (old, new_) -> 
        Some (Format.sprintf "%s:%s->%s" lbl (pr old) (pr new_))

  let string_of_output (out : output) : string = 
    let mods =
      [ match_modify_to_string dlAddr_to_string "DlSrc" out.outDlSrc;
        match_modify_to_string dlAddr_to_string "DlDst" out.outDlDst;
        match_modify_to_string dlVlan_to_string "DlVlan" out.outDlVlan;
        match_modify_to_string dlVlanPcp_to_string "DlVlanPcp" out.outDlVlanPcp;
        match_modify_to_string string_of_ip "NwSrc" out.outNwSrc;
        match_modify_to_string string_of_ip "NwDst" out.outNwDst;
        match_modify_to_string nwTos_to_string "NwTos" out.outNwTos;
        match_modify_to_string string_of_int "TpSrc" out.outTpSrc;
        match_modify_to_string string_of_int "TpDst" out.outTpDst ] in
    let mods = String.concat ", " (List.fold_right (fun xo acc -> match xo with None -> acc | Some x -> x::acc) mods []) in
    if mods = "" then
      Format.sprintf "Fwd %s"
        (match out.outPort with None -> "None" | Some p -> "Some " ^ NetCore_Pattern.string_of_port p)
    else 
      Format.sprintf "Fwd %s<%s>"
        (match out.outPort with None -> "None" | Some p -> "Some " ^ NetCore_Pattern.string_of_port p)
        mods

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
    [ { unmodified with outPort = Some (NetCore_Pattern.Physical pt) } ]

  let to_all = [ { unmodified with outPort = Some NetCore_Pattern.All } ]

  let bucket n b =
    [ { unmodified with outPort = Some (NetCore_Pattern.Bucket (n,b)) } ]

  let updateDlSrc od nw =
    [ { unmodified with outDlSrc = Some (od, nw) } ]

  let updateDlDst od nw =
    [ { unmodified with outDlDst = Some (od, nw) } ]

  let updateDlVlan od nw =
    [ { unmodified with outDlVlan = Some (od,nw) } ]

  let updateSrcIP old new_ = 
    [ { unmodified with outNwSrc = Some (old, new_) } ]

  let updateDstIP old new_ = 
    [ { unmodified with outNwDst = Some (old, new_) } ]

  let updateSrcPort old new_ = 
    [ { unmodified with outTpSrc = Some (old, new_) } ]

  let updateDstPort old new_ = 
    [ { unmodified with outTpDst = Some (old, new_) } ]

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
    concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1

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
      if NetCore_Pattern.is_empty (NetCore_Pattern.inter (build_singleton nw) pat) then
        NetCore_Pattern.empty
      else
        set_wild pat
    | None -> pat

  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> NetCore_Pattern.all

  let restrict_port portMod pat2 = match portMod with
    | Some port ->
      if NetCore_Pattern.is_empty (NetCore_Pattern.inter (NetCore_Pattern.inPort port) pat2) then
        NetCore_Pattern.empty
      else
        NetCore_Pattern.wildcardPort pat2
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
    let open NetCore_Pattern in
    restrict_port out.outPort
      (trans out.outDlSrc dlSrc wildcardDlSrc
        (trans out.outDlDst dlDst wildcardDlDst
          (trans out.outDlVlan dlVlan wildcardDlVlan pat)))

  let domain out =
    fold_right
      NetCore_Pattern.inter
      [ sel NetCore_Pattern.dlSrc out.outDlSrc
      ; sel NetCore_Pattern.dlDst out.outDlDst
      ; sel NetCore_Pattern.dlVlan out.outDlVlan ]
      NetCore_Pattern.all

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
    | Some NetCore_Pattern.All -> modify out @ (Output PseudoPort.AllPorts)
      :: unmodify out
    | Some (NetCore_Pattern.Physical pt) ->
      modify out @
        (( match inp with
         | Some pt' when (=) pt' pt ->
             Output PseudoPort.InPort
         | _ ->
           Output (PseudoPort.PhysicalPort pt)) ::
          (unmodify out))
    | Some (NetCore_Pattern.Bucket (n, true)) ->
      [ Output (PseudoPort.Controller 65535) ]
    | Some (NetCore_Pattern.Bucket (_, false)) -> []
    | None -> []

  let as_actionSequence inp act =
    concat_map (output_to_of inp) act

  let apply_action act ptpk =
    filter_map (fun a -> apply_atom a ptpk) act

  (* TODO(arjun): What if they are permutations? *)
  let is_equal x y = x = y
end
