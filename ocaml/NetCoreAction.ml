open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open Pattern
open Word
open Misc

module Port = struct
  type port =
  | Here
  | Physical of portId
  | Bucket of int
     
  let opt_portId = function
  | Physical pt -> Some pt
  | _ -> None

  let to_string p =
    match p with
    | Here -> "Here"
    | Physical pid -> "P" ^ (portId_to_string pid)
    | Bucket n -> "B" ^ (string_of_int n)
  
  type t = port
 end

module Action = struct
  module Pattern = Pattern.Make(Port)
  
  type 'a match_modify = ('a * 'a) option
  
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

  let output_to_string 
    { outDlSrc=outDlSrc
    ; outDlDst=outDlDst
    ; outDlVlan=outDlVlan
    ; outDlVlanPcp=outDlVlanPcp
    ; outNwSrc=outNwSrc
    ; outNwDst=outNwDst
    ; outNwTos=outNwTos
    ; outTpSrc=outTpSrc
    ; outTpDst=outTpDst
    ; outPort=outPort
    } =
    let opt_to_string to_string opt =
      match opt with
      | Some v -> to_string v
      | None -> ""
      in
    let reflections : (string * string) list = 
      [ ("DlSrc", opt_to_string (string_of_pair dlAddr_to_string) outDlSrc)
      ; ("DlDst", opt_to_string (string_of_pair dlAddr_to_string) outDlDst)
      ; ("DlVlan", opt_to_string (string_of_pair dlVlan_to_string) outDlVlan)
      ; ("DlVlanPcp", opt_to_string
         (string_of_pair dlVlanPcp_to_string) outDlVlanPcp)
      ; ("NwSrc", opt_to_string (string_of_pair nwAddr_to_string) outNwSrc)
      ; ("NwDst", opt_to_string (string_of_pair nwAddr_to_string) outNwDst)
      ; ("NwTos", opt_to_string (string_of_pair nwTos_to_string) outNwTos)
      ; ("TpSrc", opt_to_string (string_of_pair tpPort_to_string) outTpSrc)
      ; ("TpDst", opt_to_string (string_of_pair tpPort_to_string) outTpDst)
      ; ("Fwd", Port.to_string outPort)
      ] in
    let nonempty = List.filter (fun (f,v) -> v <> "") reflections in
    let rvs = List.map (fun (f,v) -> Printf.sprintf "%s %s" f v) nonempty in
    "{" ^ (String.concat ", " rvs) ^ "}"

  let to_string output_list =
    "[" ^ 
      (String.concat ", " (List.map output_to_string output_list)) ^ 
      "]"
  
  let outDlSrc x = x.outDlSrc
    
  let outDlDst x = x.outDlDst
  
  let outDlVlan x = x.outDlVlan
  
  let outDlVlanPcp x = x.outDlVlanPcp
  
  let outNwSrc x = x.outNwSrc
  
  let outNwDst x = x.outNwDst
  
  let outNwTos x = x.outNwTos
  
  let outTpSrc x = x.outTpSrc
  
  let outTpDst x = x.outTpDst
  
  let outPort x = x.outPort
  
  type act = output list
  
  let drop = []
  
  let pass =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = Port.Here } :: []
    
  let forward pt =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = (Port.Physical pt) } :: []
  
  let bucket n =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = (Port.Bucket n) } :: []
  
  let updateDlSrc old new0 =
    { outDlSrc = (Some (old, new0)); outDlDst = None; outDlVlan = None;
      outDlVlanPcp = None; outNwSrc = None; outNwDst = None; outNwTos = None;
      outTpSrc = None; outTpDst = None; outPort = Port.Here } :: []

  let updateDlDst old new0 =
    { outDlSrc = None; outDlDst = (Some (old, new0)); outDlVlan = None;
      outDlVlanPcp = None; outNwSrc = None; outNwDst = None; outNwTos = None;
      outTpSrc = None; outTpDst = None; outPort = Port.Here } :: []
  
  let updateDlVlan old new0 =
    { outDlSrc = None; outDlDst = None; outDlVlan = (Some (old, new0));
      outDlVlanPcp = None; outNwSrc = None; outNwDst = None; outNwTos = None;
      outTpSrc = None; outTpDst = None; outPort = Port.Here } :: []

  let par_action act1 act2 = act1 @ act2
  
  let seq_mod beq0 m1 m2 =
    match m1 with
    | Some p ->
      let (x, y) = p in
      (match m2 with
       | Some p0 ->
         let (v3, v4) = p0 in if beq0 y v3 then Some (Some (x, v4)) else None
       | None -> Some m1)
    | None -> Some m2
  
  let seq_port pt1 pt2 =
    match pt1 with
    | Port.Here -> pt2
    | _ ->
      (match pt2 with
       | Port.Here -> pt1
       | _ -> pt2)
  
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
    let { outDlSrc = dlSrc1; outDlDst = dlDst1; outDlVlan = dlVlan1;
      outDlVlanPcp = dlVlanPcp1; outNwSrc = nwSrc1; outNwDst = nwDst1;
      outNwTos = nwTos1; outTpSrc = tpSrc1; outTpDst = tpDst1; outPort =
      pt1 } = out1
    in
    let { outDlSrc = dlSrc2; outDlDst = dlDst2; outDlVlan = dlVlan2;
      outDlVlanPcp = dlVlanPcp2; outNwSrc = nwSrc2; outNwDst = nwDst2;
      outNwTos = nwTos2; outTpSrc = tpSrc2; outTpDst = tpDst2; outPort =
      pt2 } = out2
    in
    let p = ((((((((seq_mod Word48.eq_dec dlSrc1 dlSrc2),
      (seq_mod Word48.eq_dec dlDst1 dlDst2)),
      (seq_mod Word16.eq_dec dlVlan1 dlVlan2)),
      (seq_mod Word8.eq_dec dlVlanPcp1 dlVlanPcp2)),
      (seq_mod Word32.eq_dec nwSrc1 nwSrc2)),
      (seq_mod Word32.eq_dec nwDst1 nwDst2)),
      (seq_mod Word8.eq_dec nwTos1 nwTos2)),
      (seq_mod Word16.eq_dec tpSrc1 tpSrc2))
    in
    let o = seq_mod Word16.eq_dec tpDst1 tpDst2 in
    let (p0, o0) = p in
    let (p1, o1) = p0 in
    let (p2, o2) = p1 in
    let (p3, o3) = p2 in
    let (p4, o4) = p3 in
    let (p5, o5) = p4 in
    let (o6, o7) = p5 in
    (match o6 with
     | Some dlSrc0 ->
       (match o7 with
        | Some dlDst0 ->
          (match o5 with
           | Some dlVlan0 ->
             (match o4 with
              | Some dlVlanPcp0 ->
                (match o3 with
                 | Some nwSrc ->
                   (match o2 with
                    | Some nwDst ->
                      (match o1 with
                       | Some nwTos0 ->
                         (match o0 with
                          | Some tpSrc ->
                            (match o with
                             | Some tpDst ->
                               Some { outDlSrc = dlSrc0; outDlDst = dlDst0;
                                 outDlVlan = dlVlan0; outDlVlanPcp =
                                 dlVlanPcp0; outNwSrc = nwSrc; outNwDst =
                                 nwDst; outNwTos = nwTos0; outTpSrc = tpSrc;
                                 outTpDst = tpDst; outPort =
                                 (seq_port pt1 pt2) }
                             | None -> None)
                          | None -> None)
                       | None -> None)
                    | None -> None)
                 | None -> None)
              | None -> None)
           | None -> None)
        | None -> None)
     | None -> None)
  
  let cross lst1 lst2 =
    concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1
  let seq_action act1 act2 =
    filter_map (fun o1o2 -> let (o1, o2) = o1o2 in seq_output o1 o2)
      (cross act1 act2)
  
  let maybe_modify newVal modifier pk =
    match newVal with
    | Some p -> let (a, v) = p in modifier pk v
    | None -> pk
  
  let withVlanNone = function
  | Some p ->
    let (o, o0) = p in
    (match o with
     | Some old ->
       (match o0 with
        | Some new0 -> Some (old, new0)
        | None -> Some (old, coq_VLAN_NONE))
     | None ->
       (match o0 with
        | Some new0 -> Some (coq_VLAN_NONE, new0)
        | None -> Some (coq_VLAN_NONE, coq_VLAN_NONE)))
  | None -> None

  let apply_atom out ptpk =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    let (p, pk) = ptpk in
    Some (outPort0,
    (maybe_modify dlSrc0 setDlSrc
      (maybe_modify dlDst0 setDlDst
        (maybe_modify dlVlan0 setDlVlan
          (maybe_modify dlVlanPcp0 setDlVlanPcp
            (maybe_modify nwSrc setNwSrc
              (maybe_modify nwDst setNwDst
                (maybe_modify nwTos0 setNwTos
                  (maybe_modify tpSrc setTpSrc
                    (maybe_modify tpDst setTpDst pk))))))))))
  
  let trans x f pat =
    match x with
    | Some p -> let (a, new0) = p in f new0 pat
    | None -> pat
  
  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> Pattern.all
  
  let restrict_range out pat =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    trans dlSrc0 Pattern.setDlSrc (trans dlDst0 Pattern.setDlDst pat)
  
  let domain out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort = pt } =
      out
    in
    fold_right Pattern.inter 
      ((sel Pattern.dlSrc dlSrc0) :: ((sel Pattern.dlDst dlDst0) :: []))
      Pattern.all  

  let set upd mk lst =
    match upd with
    | Some p -> let (a, new0) = p in (mk new0) :: lst
    | None -> lst
  
  let unset upd mk lst =
    match upd with
    | Some p -> let (old, y) = p in (mk old) :: lst
    | None -> lst
  
  let setDlVlan' = function
  | Some n -> SetDlVlan n
  | None -> StripVlan
  
  let modify out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    set dlSrc0 (fun x -> SetDlSrc x)
      (set dlDst0 (fun x -> SetDlDst x)
        (set dlVlan0 (fun x -> SetDlVlan x)
          (set dlVlanPcp0 (fun x -> SetDlVlanPcp x)
            (set nwSrc (fun x -> SetNwSrc x)
              (set nwDst (fun x -> SetNwDst x)
                (set nwTos0 (fun x -> SetNwTos x)
                  (set tpSrc (fun x -> SetTpSrc x)
                    (set tpDst (fun x -> SetTpDst x) []))))))))
  
  let unmodify out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    unset dlSrc0 (fun x -> SetDlSrc x)
      (unset dlDst0 (fun x -> SetDlDst x)
        (unset dlVlan0 (fun x -> SetDlVlan x)
          (unset dlVlanPcp0 (fun x -> SetDlVlanPcp x)
            (unset nwSrc (fun x -> SetNwSrc x)
              (unset nwDst (fun x -> SetNwDst x)
                (unset nwTos0 (fun x -> SetNwTos x)
                  (unset tpSrc (fun x -> SetTpSrc x)
                    (unset tpDst (fun x -> SetTpDst x) []))))))))
  
  let output_to_of inp out =
    match out.outPort with
    | Port.Physical pt ->
       (modify out) @
        ((match inp with
          | Some pt' ->
            if Word16.eq_dec pt' pt
            then Output InPort
            else Output (PhysicalPort pt)
          | None -> Output (PhysicalPort pt)) :: (unmodify out))
    | Port.Here -> []
    | Port.Bucket n -> (Output (Controller Word16.max_value)) :: []
  
  let as_actionSequence inp action0 =
    concat_map (output_to_of inp) action0
  
  type t = act
  
  type e = output
  
  type pattern = Pattern.t
  
  type port = Port.t
  
  let atoms action0 =
    action0
  
  let apply_action action0 ptpk =
    filter_map (fun a -> apply_atom a ptpk) action0
 end

