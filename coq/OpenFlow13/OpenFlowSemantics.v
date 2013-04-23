Set Implicit Arguments.

Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import OpenFlow13.OpenFlow0x01Types.

Local Open Scope list_scope.
Local Open Scope bool_scope.

Section Actions.

(** Note that dlTyp is an implicit argument (determined by the index of nw. *)
  Definition setVlan dlVlan pkt := 
    match pkt with
      | Packet dlSrc dlDst _ _ dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp nw
    end.

  Definition setVlanPriority dlVlanPcp pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan _ nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp nw
    end.

  Definition stripVlanHeader pkt :=
    match pkt with
      | Packet dlSrc dlDst _ dlVlan _ nw =>
        Packet dlSrc dlDst VLAN_NONE Word8.zero nw
    end.

  Definition setEthSrcAddr dlSrc pkt := 
    match pkt with
      | Packet _ dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp nw
    end.

  Definition setEthDstAddr dlDst pkt := 
    match pkt with
      | Packet dlSrc _ _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp nw
    end.

  Definition setIPSrcAddr_nw (ethTyp : dlTyp) (src : nwAddr)
    (pkt : nw ethTyp) : nw ethTyp := 
    match pkt with
      | NwUnparsable pf data => 
        NwUnparsable pf data
      | NwIP (IP vhl tos len ident flags frag ttl _ chksum _ dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl chksum src dst tp)
      | NwARP (ARPQuery sha _ tpa) =>
        NwARP (ARPQuery sha src tpa)
      | NwARP (ARPReply sha _ tha tpa) =>
        NwARP (ARPReply sha src tha tpa)
    end.

  Definition setIPSrcAddr nwSrc pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp  (setIPSrcAddr_nw nwSrc nw)
    end.

  Definition setIPDstAddr_nw (ethTyp : dlTyp) (dst : nwAddr) 
    (pkt : nw ethTyp) : nw ethTyp := 
    match pkt with
      | NwUnparsable pf data => 
        NwUnparsable pf data
      | NwIP (IP vhl tos len ident flags frag ttl _ chksum src _ tp) =>
        NwIP (IP vhl tos len ident flags frag ttl chksum src dst tp)
      | NwARP (ARPQuery sha spa _) =>
        NwARP (ARPQuery sha spa dst)
      | NwARP (ARPReply sha spa tha _) =>
        NwARP (ARPReply sha spa tha dst)
    end.

  Definition setIPDstAddr nwDst pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp  (setIPDstAddr_nw nwDst nw)
    end.

  Definition setIPToS_nw (ethTyp : dlTyp) (tos : nwTos)
    (pkt : nw ethTyp) : nw ethTyp := 
    match pkt with
      | NwUnparsable pf data => 
        NwUnparsable pf data
      | NwIP (IP vhl _ len ident flags frag ttl _ chksum src dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl chksum src dst tp)
      | NwARP (ARPQuery dlSrc nwSrc nwDst) =>
        NwARP (ARPQuery dlSrc nwSrc nwDst)
      | NwARP (ARPReply dlSrc nwSrc dlDst nwDst) =>
        NwARP (ARPReply dlSrc nwSrc dlDst nwDst)
    end.

  Definition setIPToS nwToS pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp  (setIPToS_nw nwToS nw)
    end.

  Definition setTransportSrcPort_tp (proto : nwProto)
    (tpSrc : tpPort) (pkt : tpPkt proto) : tpPkt proto :=
    match pkt with
      | TpTCP (Tcp _ dst seq ack off flags win payload) =>
        TpTCP (Tcp tpSrc dst seq ack off flags win payload)
      | TpICMP icmp => TpICMP icmp (* TODO(arjun): should set *)
      | TpUnparsable proto data => TpUnparsable proto data
    end.

  Definition setTransportSrcPort_nw (ethTyp : dlTyp) 
    (tpSrc : tpPort) (pkt : nw ethTyp) : nw ethTyp := 
    match pkt with
      | NwUnparsable pf data => 
        NwUnparsable pf data
      | NwIP (IP vhl tos len ident flags frag ttl _ chksum src dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl chksum src dst 
	         (setTransportSrcPort_tp tpSrc tp))
      | NwARP arp => 
        NwARP arp
    end.

  Definition setTransportSrcPort tpSrc pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp  
        (setTransportSrcPort_nw tpSrc nw)
    end.

  Definition setTransportDstPort_tp (proto : nwProto)
    (tpDst : tpPort) (pkt : tpPkt proto) : tpPkt proto :=
    match pkt with
      | TpTCP (Tcp src _ seq ack off flags win payload) =>
        TpTCP (Tcp src tpDst seq ack off flags win payload)
      | TpICMP icmp => TpICMP icmp (* TODO(arjun): should set *)
      | TpUnparsable proto data => TpUnparsable proto data
    end.

  Definition setTransportDstPort_nw (ethTyp : dlTyp)
    (tpDst : tpPort) (pkt : nw ethTyp) : nw ethTyp := 
    match pkt with
      | NwUnparsable pf data => NwUnparsable pf data
      | NwIP (IP vhl tos len ident flags frag ttl _ chksum src dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl chksum src dst 
	         (setTransportDstPort_tp tpDst tp))
      | NwARP arp => NwARP arp
    end.

  Definition setTransportDstPort tpDst pkt := 
    match pkt with
      | Packet dlSrc dlDst _ dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlVlan dlVlanPcp  
        (setTransportSrcPort_nw tpDst nw)
    end.

  Inductive action_result : Type :=
  | arPort : pseudoPort -> action_result
  | arPkt : packet -> action_result
  | arGroup : groupId -> action_result.

  Definition apply_action (pt : portId) (pk : packet) (act : action) :=
    match act with
      | Output pp => arPort pp
      | Group gid => arGroup gid
      | SetDlVlan vlan => arPkt (setVlan vlan pk)
      | StripVlan => arPkt (stripVlanHeader pk)
      | SetDlVlanPcp prio => arPkt (setVlanPriority prio pk)
      | SetDlSrc addr => arPkt (setEthSrcAddr addr pk)
      | SetDlDst addr => arPkt (setEthDstAddr addr pk)
      | SetNwSrc ip => arPkt (setIPSrcAddr ip pk)
      | SetNwDst ip => arPkt (setIPDstAddr ip pk)
      | SetNwTos tos => arPkt (setIPToS tos pk)
      | SetTpSrc pt => arPkt (setTransportSrcPort pt pk)
      | SetTpDst pt => arPkt (setTransportDstPort pt pk)
    end.

  Fixpoint apply_actionSequence (pt : portId) (pk : packet) 
    (acts : actionSequence) :  list ((pseudoPort + groupId) * packet) :=
    match acts with
      | nil => nil
      | act :: acts' =>
        match apply_action pt pk act with
          | arPort pp => (inl pp,pk) :: apply_actionSequence pt pk acts'
          | arGroup gid => (inr gid,pk) :: apply_actionSequence pt pk acts'
          | arPkt pk' => apply_actionSequence pt pk' acts'
        end
    end.

End Actions.

Section Match.

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
          | TpUnparsable _ _ => 
            true
        end
    end.

  Definition match_nw (ethTyp : dlTyp) (pk : nw ethTyp) 
    (mat : of_match) :=
    match mat with
      | Match _ _ _ _ _ mNwSrc mNwDst mNwProto mNwTos _ _ _ =>
        match pk with
          | NwIP (IP _ nwTos _ _ _ _ _ nwProto _ nwSrc nwDst tpPkt) => 
            match_opt Word32.eq_dec nwSrc mNwSrc &&
            match_opt Word32.eq_dec nwDst mNwDst &&
            match_opt Word8.eq_dec nwTos mNwTos &&
            match mNwProto with
              | None => true
              | Some nwProto' => 
                if Word8.eq_dec nwProto nwProto' then
                  match_tp tpPkt mat
                else
                  false
            end
          | NwARP (ARPQuery _ nwSrc nwDst) => 
            match_opt Word32.eq_dec nwSrc mNwSrc &&
            match_opt Word32.eq_dec nwDst mNwDst
          | NwARP (ARPReply _ nwSrc _ nwDst) => 
            match_opt Word32.eq_dec nwSrc mNwSrc &&
            match_opt Word32.eq_dec nwDst mNwDst
          | NwUnparsable _ _ => true (* really? *)
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

End Match.
