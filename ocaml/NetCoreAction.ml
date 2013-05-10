open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open Pattern
open WordInterface
open Misc

module Port = struct
  type port =
  | Here
  | Physical of portId
  | Bucket of int
     
  let opt_portId = function
  | Physical pt -> Some pt
  | _ -> None
  
  type t = port
 end

module type NETCORE_ACTION = sig 
  module Pattern : PATTERN
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> pattern
  
  val forward : portId -> t
  
  val bucket : int -> t
  
  val updateDlSrc : dlAddr -> dlAddr -> t
  
  val as_actionSequence : portId option -> t -> actionSequence
 end

module NetCoreAction = struct
  module Pattern = Pattern.Make(Port)
  
  type 'a match_modify = ('a * 'a) option
  
  type output = { outDlSrc : dlAddr match_modify;
                  outDlDst : dlAddr match_modify;
                  outDlVlan : dlVlan option match_modify;
                  outDlVlanPcp : dlVlanPcp match_modify;
                  outNwSrc : nwAddr match_modify;
                  outNwDst : nwAddr match_modify;
                  outNwTos : nwTos match_modify;
                  outTpSrc : tpPort match_modify;
                  outTpDst : tpPort match_modify; outPort : Pattern.port }
    
  (** val outDlSrc : output -> dlAddr match_modify **)
  
  let outDlSrc x = x.outDlSrc
  
  (** val outDlDst : output -> dlAddr match_modify **)
  
  let outDlDst x = x.outDlDst
  
  (** val outDlVlan : output -> dlVlan option match_modify **)
  
  let outDlVlan x = x.outDlVlan
  
  (** val outDlVlanPcp : output -> dlVlanPcp match_modify **)
  
  let outDlVlanPcp x = x.outDlVlanPcp
  
  (** val outNwSrc : output -> nwAddr match_modify **)
  
  let outNwSrc x = x.outNwSrc
  
  (** val outNwDst : output -> nwAddr match_modify **)
  
  let outNwDst x = x.outNwDst
  
  (** val outNwTos : output -> nwTos match_modify **)
  
  let outNwTos x = x.outNwTos
  
  (** val outTpSrc : output -> tpPort match_modify **)
  
  let outTpSrc x = x.outTpSrc
  
  (** val outTpDst : output -> tpPort match_modify **)
  
  let outTpDst x = x.outTpDst
  
  (** val outPort : output -> Pattern.port **)
  
  let outPort x = x.outPort
  
  type act = output list
  
  (** val drop : act **)
  
  let drop =
    []
  
  (** val pass : output list **)
  
  let pass =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = Port.Here } :: []
  
  (** val forward : portId -> output list **)
  
  let forward pt =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = (Port.Physical pt) } :: []
  
  (** val bucket : int -> output list **)
  
  let bucket n =
    { outDlSrc = None; outDlDst = None; outDlVlan = None; outDlVlanPcp =
      None; outNwSrc = None; outNwDst = None; outNwTos = None; outTpSrc =
      None; outTpDst = None; outPort = (Port.Bucket n) } :: []
  
  (** val updateDlSrc : dlAddr -> dlAddr -> output list **)
  
  let updateDlSrc old new0 =
    { outDlSrc = (Some (old, new0)); outDlDst = None; outDlVlan = None;
      outDlVlanPcp = None; outNwSrc = None; outNwDst = None; outNwTos = None;
      outTpSrc = None; outTpDst = None; outPort = Port.Here } :: []
  
  (** val par_action : act -> act -> act **)
  
  let par_action act1 act2 =
     act1 @ act2
  
  (** val seq_mod :
      ('a1 -> 'a1 -> bool) -> 'a1 match_modify -> 'a1 match_modify ->
      ('a1 * 'a1) option option **)
  
  let seq_mod beq0 m1 m2 =
    match m1 with
    | Some p ->
      let (x, y) = p in
      (match m2 with
       | Some p0 ->
         let (v3, v4) = p0 in if beq0 y v3 then Some (Some (x, v4)) else None
       | None -> Some m1)
    | None -> Some m2
  
  (** val seq_port : Pattern.port -> Pattern.port -> Pattern.port **)
  
  let seq_port pt1 pt2 =
    match pt1 with
    | Port.Here -> pt2
    | _ ->
      (match pt2 with
       | Port.Here -> pt1
       | _ -> pt2)
  
  (** val optword16beq : Word16.t option -> Word16.t option -> bool **)
  
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
  
  (** val seq_output : output -> output -> output option **)
  
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
      (seq_mod optword16beq dlVlan1 dlVlan2)),
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
  
  (** val cross : 'a1 list -> 'a2 list -> ('a1 * 'a2) list **)
  
  let cross lst1 lst2 =
    concat_map (fun a -> map (fun b -> (a, b)) lst2) lst1
  
  (** val seq_action : act -> act -> act **)
  
  let seq_action act1 act2 =
    filter_map (fun o1o2 -> let (o1, o2) = o1o2 in seq_output o1 o2)
      (cross act1 act2)
  
  (** val maybe_modify :
      'a1 match_modify -> (packet -> 'a1 -> packet) -> packet -> packet **)
  
  let maybe_modify newVal modifier pk =
    match newVal with
    | Some p -> let (a, v) = p in modifier pk v
    | None -> pk
  
  (** val withVlanNone :
      dlVlan option match_modify -> (dlVlan * dlVlan) option **)
  
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
  
  (** val apply_atom :
      output -> (Pattern.port * packet) -> (Pattern.port * packet) option **)
  
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
        (maybe_modify (withVlanNone dlVlan0) setDlVlan
          (maybe_modify dlVlanPcp0 setDlVlanPcp
            (maybe_modify nwSrc setNwSrc
              (maybe_modify nwDst setNwDst
                (maybe_modify nwTos0 setNwTos
                  (maybe_modify tpSrc setTpSrc
                    (maybe_modify tpDst setTpDst pk))))))))))
  
  (** val trans :
      'a1 match_modify -> ('a1 -> Pattern.t -> Pattern.t) -> Pattern.t ->
      Pattern.t **)
  
  let trans x f pat =
    match x with
    | Some p -> let (a, new0) = p in f new0 pat
    | None -> pat
  
  (** val sel : ('a1 -> Pattern.t) -> 'a1 match_modify -> Pattern.t **)
  
  let sel f = function
  | Some p -> let (old, y) = p in f old
  | None -> Pattern.all
  
  (** val restrict_range : output -> Pattern.t -> Pattern.t **)
  
  let restrict_range out pat =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    trans dlSrc0 Pattern.setDlSrc (trans dlDst0 Pattern.setDlDst pat)
  
  (** val domain : output -> Pattern.pattern **)
  
  let domain out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort = pt } =
      out
    in
    fold_right Pattern.inter 
      ((sel Pattern.dlSrc dlSrc0) :: ((sel Pattern.dlDst dlDst0) :: []))
      Pattern.all  
  (** val set :
      'a1 match_modify -> ('a1 -> action) -> actionSequence -> action list **)
  
  let set upd mk lst =
    match upd with
    | Some p -> let (a, new0) = p in (mk new0) :: lst
    | None -> lst
  
  (** val unset :
      'a1 match_modify -> ('a1 -> action) -> actionSequence -> action list **)
  
  let unset upd mk lst =
    match upd with
    | Some p -> let (old, y) = p in (mk old) :: lst
    | None -> lst
  
  (** val setDlVlan' : dlVlan option -> action **)
  
  let setDlVlan' = function
  | Some n -> SetDlVlan n
  | None -> StripVlan
  
  (** val modify : output -> actionSequence **)
  
  let modify out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    set dlSrc0 (fun x -> SetDlSrc x)
      (set dlDst0 (fun x -> SetDlDst x)
        (set dlVlan0 setDlVlan'
          (set dlVlanPcp0 (fun x -> SetDlVlanPcp x)
            (set nwSrc (fun x -> SetNwSrc x)
              (set nwDst (fun x -> SetNwDst x)
                (set nwTos0 (fun x -> SetNwTos x)
                  (set tpSrc (fun x -> SetTpSrc x)
                    (set tpDst (fun x -> SetTpDst x) []))))))))
  
  (** val unmodify : output -> actionSequence **)
  
  let unmodify out =
    let { outDlSrc = dlSrc0; outDlDst = dlDst0; outDlVlan = dlVlan0;
      outDlVlanPcp = dlVlanPcp0; outNwSrc = nwSrc; outNwDst = nwDst;
      outNwTos = nwTos0; outTpSrc = tpSrc; outTpDst = tpDst; outPort =
      outPort0 } = out
    in
    unset dlSrc0 (fun x -> SetDlSrc x)
      (unset dlDst0 (fun x -> SetDlDst x)
        (unset dlVlan0 setDlVlan'
          (unset dlVlanPcp0 (fun x -> SetDlVlanPcp x)
            (unset nwSrc (fun x -> SetNwSrc x)
              (unset nwDst (fun x -> SetNwDst x)
                (unset nwTos0 (fun x -> SetNwTos x)
                  (unset tpSrc (fun x -> SetTpSrc x)
                    (unset tpDst (fun x -> SetTpDst x) []))))))))
  
  (** val output_to_of : portId option -> output -> actionSequence **)
  
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
  
  (** val as_actionSequence : portId option -> act -> action list **)
  
  let as_actionSequence inp action0 =
    concat_map (output_to_of inp) action0
  
  type t = act
  
  type e = output
  
  type pattern = Pattern.t
  
  type port = Port.t
  
  (** val atoms : t -> e list **)
  
  let atoms action0 =
    action0
  
  (** val apply_action :
      t -> (Port.t * packet) -> (Pattern.port * packet) list **)
  
  let apply_action action0 ptpk =
    filter_map (fun a -> apply_atom a ptpk) action0
 end

