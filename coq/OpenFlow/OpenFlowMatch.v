Set Implicit Arguments.

Require Import Bool.Bool.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.Packet.
Require Import OpenFlow.MessagesDef.

Definition match_opt {A : Type} (eq_dec : Eqdec A) (x : A) (v : option A) :=
  match v with
    | None => true
    | Some y => if eq_dec x y then true else false
  end.

Definition match_tp (nwProto : nwProto) (pk : tpPkt nwProto)
                    (mat : of_match) :=
  match mat with
    | Match _ _ _ _ _ _ _ _ _ mTpSrc mTpDst _ =>
      match pk with
        | TpTCP (Tcp tpSrc tpDst _ _ _ _ _ _) => 
            match_opt Word16.eq_dec tpSrc mTpSrc &&
            match_opt Word16.eq_dec tpDst mTpDst
        | TpICMP (Icmp typ code _ _) => 
            true
            (* TODO(arjun): this is wrong *)
            (* match_opt Word16.eq_dec typ mTpSrc &&
            match_opt Word16.eq_dec code mTpDst *)
        | TpUnparsable _ => 
          true
      end
  end.

Definition match_nw (ethTyp : dlTyp) (pk : nw ethTyp) 
                    (mat : of_match) :=
  match mat with
    | Match _ _ _ _ _ mNwSrc mNwDst mNwProto mNwTos _ _ _ =>
      match pk with
        | NwIP (IP _ _ nwSrc nwDst nwTos nwProto _ _ _ dlPkt) => 
          match_opt Word32.eq_dec nwSrc mNwSrc &&
          match_opt Word32.eq_dec nwDst mNwDst &&
          match_opt Word8.eq_dec nwTos mNwTos &&
          match mNwProto with
            | None => true
            | Some nwProto' => 
              if Word8.eq_dec nwProto nwProto' then
                match_tp dlPkt mat
              else
                false
          end
        | NwARP (ARPQuery _ nwSrc nwDst) => 
          match_opt Word32.eq_dec nwSrc mNwSrc &&
          match_opt Word32.eq_dec nwDst mNwDst
        | NwARP (ARPReply _ nwSrc _ nwDst) => 
          match_opt Word32.eq_dec nwSrc mNwSrc &&
          match_opt Word32.eq_dec nwDst mNwDst
        | NwUnparsable _ => true (* really? *)
      end
  end.

Definition match_ethFrame (pk : packet) (pt : portId) (mat : of_match) :=
  match mat with
    | Match mDlSrc mDlDst mDlTyp mDlVlan mDlVlanPcp mNwSrc mNwDst mNwProto
              mNwTos mTpSrc mTpDst mInPort =>
      match_opt Word16.eq_dec pt mInPort &&
      match pk with
        | Packet pkDlSrc pkDlDst pkDlTyp pkDlVlan pkDlVlanPcp pkNwFrame =>
          match_opt Word48.eq_dec pkDlSrc mDlSrc &&
          match_opt Word48.eq_dec pkDlDst mDlDst &&
          match_opt Word16.eq_dec pkDlVlan mDlVlan &&
          match_opt Word8.eq_dec pkDlVlanPcp mDlVlanPcp &&
          match mDlTyp with
            | None => true
            | Some dlTyp' =>
              if Word16.eq_dec pkDlTyp dlTyp' then
                match_nw pkNwFrame mat
              else
                false
          end
      end
  end.

