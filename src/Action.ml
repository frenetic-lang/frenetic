open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open Pattern
open Word
open Misc

module Bool = struct
  type t = bool
  type e = bool
  let atoms b = [b]
    
  let drop = false
  let pass = true
    
  let apply_atom b ptpk = if b then Some ptpk else None
  
  let apply_action action ptpk =
    filter_map (fun a -> apply_atom a ptpk) (atoms action)
  
  let par_action b1 b2 =
    (||) b1 b2
  
  let seq_action b1 b2 =
    (&&) b1 b2
  
  let restrict_range b p = p
  
  let domain b = Pattern.all
end

module Output = struct
  
  type 'a match_modify = ('a * 'a) option
  
  (* JNF: why can't we modify DlType, NwProto? Worried about validity of patterns? *)
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
  
  let outDlSrc out = out.outDlSrc
    
  let outDlDst out = out.outDlDst
  
  let outDlVlan out = out.outDlVlan
  
  let outDlVlanPcp out = out.outDlVlanPcp
  
  let outNwSrc out = out.outNwSrc
  
  let outNwDst out = out.outNwDst
  
  let outNwTos out = out.outNwTos
  
  let outTpSrc out = out.outTpSrc
  
  let outTpDst out = out.outTpDst
  
  let outPort out = out.outPort
  
  type e = output

  type t = output list
  
  let atoms act = act
  
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
  
  let restrict_range out pat =
    trans out.outDlSrc Pattern.setDlSrc (trans out.outDlDst Pattern.setDlDst pat)
  
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

