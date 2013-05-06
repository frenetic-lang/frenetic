(** WARNING(arjun): Does not compile in Proof General. Proof General freezes
    on Pattern.inter and ValidPattern. coqc does work.
 *)
Set Implicit Arguments.

Require Import Coq.Arith.EqNat.
Require Import NPeano.
Require Import Arith.Peano_dec.
Require Import Bool.Bool.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Common.Monad.

Require Import OpenFlow.OpenFlow0x01Types.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import Wildcard.Wildcard.
Require Import Pattern2.PatternSignatures.


Local Open Scope bool_scope.
Local Open Scope list_scope.

Module Make (Port : PORT).
  
  Definition port := Port.t.
  

Record pattern : Type := Pattern {
  ptrnDlSrc : Wildcard dlAddr;
  ptrnDlDst : Wildcard dlAddr;
  ptrnDlType : Wildcard dlTyp;
  ptrnDlVlan : Wildcard dlVlan;
  ptrnDlVlanPcp : Wildcard dlVlanPcp;
  ptrnNwSrc : Wildcard nwAddr;
  ptrnNwDst : Wildcard nwAddr;
  ptrnNwProto : Wildcard nwProto;
  ptrnNwTos : Wildcard nwTos;
  ptrnTpSrc : Wildcard tpPort;
  ptrnTpDst : Wildcard tpPort;
  ptrnInPort : Wildcard port
}.

Lemma eq_dec : forall (x y : pattern), { x = y } + { x <> y }.
Proof.
  decide equality;
    try solve [ apply (Wildcard.eq_dec Word16.eq_dec) |
      apply (Wildcard.eq_dec Word32.eq_dec) |
        apply (Wildcard.eq_dec Word8.eq_dec) |
          apply (Wildcard.eq_dec Word48.eq_dec) ].
  apply (Wildcard.eq_dec Port.eqdec).
Defined.

Definition all :=
  Pattern
    WildcardAll WildcardAll WildcardAll WildcardAll WildcardAll 
    WildcardAll WildcardAll WildcardAll WildcardAll WildcardAll
    WildcardAll WildcardAll.

Definition empty :=
  Pattern WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone.

(** Note that we do not have a unique representation for empty patterns! *)
Definition is_empty pat : bool :=
  match pat with
    | Pattern dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst
      inPort =>
      Wildcard.is_empty inPort ||
        Wildcard.is_empty dlSrc ||
          Wildcard.is_empty dlDst ||
            Wildcard.is_empty vlan ||
              Wildcard.is_empty pcp || 
                Wildcard.is_empty typ ||
                  Wildcard.is_empty nwSrc || 
                    Wildcard.is_empty nwDst || 
                      Wildcard.is_empty nwTos || 
                        Wildcard.is_empty nwProto ||
                          Wildcard.is_empty tpSrc || 
                            Wildcard.is_empty tpDst
  end.

  Section ToOpenFlow0x01.

    Definition wild_to_opt {A : Type} (w : Wildcard A) : option (option A) :=
      match w with
        | WildcardAll => Some None
        | WildcardExact x => Some (Some x)
        | WildcardNone => None
      end.

    Import Common.Monad.
    Local Notation "x <- M ; K" := (Maybe.bind M (fun x => K)). 

    Definition to_match (pat : pattern) :=
      match pat with
        | Pattern dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst pt =>
            dlSrc <- wild_to_opt dlSrc;
            dlDst <- wild_to_opt dlDst;
            typ <- wild_to_opt typ;
            vlan <- wild_to_opt vlan;
            pcp <- wild_to_opt pcp;
            nwSrc <- wild_to_opt nwSrc;
            nwDst <- wild_to_opt nwDst;
            nwProto <- wild_to_opt nwProto;
            nwTos <- wild_to_opt nwTos;
            tpSrc <- wild_to_opt tpSrc;
            tpDst <- wild_to_opt tpDst;
            pt <- wild_to_opt pt; (* fail if matching on no port *)
            pt <- (match pt with
                       | None => Some None (* not matching on a port at all is OK *)
                       | Some pt => 
                         match Port.opt_portId pt with
                           | Some phys => Some (Some phys)
                           | None => None
                         end (* if matching, it must be a physical port *)
                   end);
            Maybe.ret (Match dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst pt)
      end.

  End ToOpenFlow0x01.

Definition inter p p' :=
  let dlSrc := Wildcard.inter Word48.eq_dec (ptrnDlSrc p) 
    (ptrnDlSrc p') in
  let dlDst := Wildcard.inter Word48.eq_dec (ptrnDlDst p) 
    (ptrnDlDst p') in
  let dlType := Wildcard.inter Word16.eq_dec (ptrnDlType p) (ptrnDlType p') in
  let dlVlan := Wildcard.inter Word16.eq_dec (ptrnDlVlan p) (ptrnDlVlan p') in
  let dlVlanPcp := Wildcard.inter Word8.eq_dec (ptrnDlVlanPcp p) 
    (ptrnDlVlanPcp p') in
   let nwSrc := Wildcard.inter Word32.eq_dec (ptrnNwSrc p) (ptrnNwSrc p') in
   let nwDst := Wildcard.inter Word32.eq_dec (ptrnNwDst p) (ptrnNwDst p') in
   let nwProto := Wildcard.inter Word8.eq_dec (ptrnNwProto p)
     (ptrnNwProto p') in
   let nwTos := Wildcard.inter Word8.eq_dec (ptrnNwTos p) (ptrnNwTos p') in
   let tpSrc := Wildcard.inter Word16.eq_dec (ptrnTpSrc p) (ptrnTpSrc p') in
   let tpDst := Wildcard.inter Word16.eq_dec (ptrnTpDst p) (ptrnTpDst p') in
   let inPort := Wildcard.inter Port.eqdec (ptrnInPort p) (ptrnInPort p') in
     Pattern dlSrc dlDst dlType dlVlan dlVlanPcp 
       nwSrc nwDst nwProto nwTos 
       tpSrc tpDst 
       inPort.

Definition exact_pattern (pk : packet) pt :=
  Pattern
  (WildcardExact (pktDlSrc pk))
  (WildcardExact (pktDlDst pk))
  (WildcardExact (pktDlTyp pk))
  (WildcardExact (pktDlVlan pk))
  (WildcardExact (pktDlVlanPcp pk))
  (WildcardExact (pktNwSrc pk))
  (WildcardExact (pktNwDst pk))
  (WildcardExact (pktNwProto pk))
  (WildcardExact (pktNwTos pk))
  (WildcardExact (pktTpSrc pk))
  (WildcardExact (pktTpDst pk))
  (WildcardExact pt).

Definition match_packet pt (pk : packet) pat :=
  negb (is_empty (inter (exact_pattern pk pt) pat)).

Definition is_exact pat := 
  match pat with
    | Pattern dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst
      inPort =>
      Wildcard.is_exact inPort &&
      Wildcard.is_exact dlSrc &&
      Wildcard.is_exact dlDst &&
      Wildcard.is_exact typ &&
      Wildcard.is_exact vlan &&
      Wildcard.is_exact pcp &&
      Wildcard.is_exact nwSrc &&
      Wildcard.is_exact nwDst &&
      Wildcard.is_exact nwProto &&
      Wildcard.is_exact nwTos &&
      Wildcard.is_exact tpSrc &&
      Wildcard.is_exact tpDst
  end.

(** TODO(arjun): ICMP is a little strange. Read spec to see how its fields
    are mapped. *)
Definition SupportedNwProto := 
  [ Const_0x6; 
    Const_0x7 ].

Definition SupportedDlTyp := 
  [ Const_0x800; Const_0x806 ].

Definition to_valid (pat : pattern) : pattern :=
  match pat with
    | Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
              nwProto nwTos tpSrc tpDst inPort =>
      let validDlTyp :=
          match dlTyp with
            | WildcardExact n => 
              match Word16.eq_dec n Const_0x800 with
                | left _ => true
                | right _ =>
                  match Word16.eq_dec n Const_0x806 with
                    | left _ => true
                    | right _ => false
                  end
              end
            | _ => false
          end in
      let validNwProto :=
          match nwProto with
            | WildcardExact n => match Word8.eq_dec n Const_0x6 with
                | left _ => true
                | right _ =>
                  match Word8.eq_dec n Const_0x7 with
                    | left _ => true
                    | right _ => false
                  end
              end
        | _ => false 
          end in
      Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp 
              (if validDlTyp then nwSrc else WildcardAll)
              (if validDlTyp then nwDst else WildcardAll)
              (if validDlTyp then nwProto else WildcardAll)
              (if validDlTyp then nwTos else WildcardAll)
              (if validNwProto then tpSrc else WildcardAll)
              (if validNwProto then tpDst else WildcardAll)
              inPort
  end.

Definition to_all {A : Type} (w : Wildcard A) (b : bool) :=
  match b with
    | true => WildcardAll
    | false => w
  end.          

Section Setters.

  Definition setDlSrc dlSrc pat := 
    match pat with
    | (Pattern _ dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
               nwProto nwTos tpSrc tpDst inPort) =>
      to_valid (Pattern (WildcardExact dlSrc)   dlDst dlTyp dlVlan dlVlanPcp 
                        nwSrc nwDst nwProto nwTos 
                        tpSrc tpDst inPort)
    end.

  Definition setDlDst dlDst pat := 
    match pat with
    | (Pattern dlSrc _ dlTyp dlVlan dlVlanPcp nwSrc nwDst
               nwProto nwTos tpSrc tpDst inPort) =>
      to_valid (Pattern dlSrc (WildcardExact dlDst) dlTyp dlVlan dlVlanPcp
                        nwSrc nwDst
                        nwProto nwTos 
                        tpSrc tpDst inPort)
    end.

End Setters.

End Make.