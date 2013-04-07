Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import FwOF.FwOFSignatures.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require FwOF.FwOFExtractableController.
Local Open Scope list_scope.

Module Make (NetAndPol : NETWORK_AND_POLICY) <: ATOMS.
  Include NetAndPol.

  Module Import ExtractableController := FwOF.FwOFExtractableController.MakeController (NetAndPol).
  Definition controller := controller.


  Inductive Recv : controller -> switchId -> fromSwitch -> controller -> Prop :=
  | RecvBarrierReply : forall st swId n,
    Recv st swId (BarrierReply n) st
  | RecvPacketIn : forall swsts pksToSend sw pt pk,
    Recv (State pksToSend swsts) 
         sw (PacketIn pt pk)
         (State (mkPktOuts sw pt pk ++ pksToSend) swsts).

  Inductive Send : state -> state -> switchId -> fromController -> Prop :=
  | SendPacketOut : forall swsts srcPt srcPk dstPt dstPk sw lps,
    Send (State ((SrcDst sw srcPt srcPk dstPt dstPk)::lps) swsts)
         (State lps swsts)
         sw
         (PacketOut dstPt dstPk)
  | SendMessage : forall sw stsws stsws' msg msgs,
      Send 
        (State nil 
               (stsws ++ (SwitchState sw (msg::msgs)) :: stsws'))
        (State nil 
               (stsws ++ (SwitchState sw msgs) :: stsws'))
        sw
        msg.

  Inductive Step : state -> state -> Prop := .
           
  Definition controller_recv := Recv.
  Definition controller_step := Step.
  Definition controller_send := Send.

  Hint Constructors Send Recv.

  Lemma Send_cons : forall s ss1 ss2 sw msg,
    Send (State nil ss1) (State nil ss2) sw msg ->
    Send (State nil (s::ss1)) (State nil (s::ss2)) sw msg.
  Proof with auto with datatypes.
    intros.
    inversion H; subst.
    do 2 rewrite -> app_comm_cons...
  Qed.

  Lemma send_queued_compat : forall ss1 ss2 sw msg,
    send_queued ss1 = Some (ss2, sw, msg) ->
    Send (State nil ss1) (State nil ss2) sw msg.
  Proof with auto with datatypes.
    intros.
    generalize dependent ss2.
    generalize dependent sw.
    generalize dependent msg.
    induction ss1; intros...
    simpl in H. inversion H.
    simpl in H.
    destruct a.
    + destruct pendingCtrlMsgs0.
      - remember (send_queued ss1) as rest.
        destruct rest.
        * destruct p.
          destruct p.
          remember (IHss1 f s l eq_refl) as J eqn:X; clear X.
          inversion H; subst.
          apply Send_cons...
        * inversion H.
      - inversion H; subst.
        assert (SwitchState sw (msg::pendingCtrlMsgs0) :: ss1 = 
                nil ++ SwitchState sw (msg::pendingCtrlMsgs0) :: ss1) as X...
        rewrite -> X; clear X.
        assert (SwitchState sw pendingCtrlMsgs0 :: ss1 = 
                nil ++ SwitchState sw pendingCtrlMsgs0 :: ss1) as X...
        rewrite -> X; clear X...
  Qed.

  Lemma send_compat : forall st1 st2 sw msg,
    send st1 = Some (st2, sw, msg) ->
    Send st1 st2 sw msg.
  Proof with auto with datatypes.
    intros.
    destruct st1.
    simpl in H.
    destruct pktsToSend0.
    + remember (send_queued switchStates0) as J.
      destruct J.
      - destruct p as [[ss2 sw2] msg2].
        symmetry in HeqJ; apply send_queued_compat in HeqJ.
        inversion H; subst...
      - inversion H.
    + destruct s.
      inversion H; subst...
  Qed.


  Lemma recv_compat : forall st1 sw msg st2,
    recv st1 sw msg = st2 ->
    Recv st1 sw msg st2.
  Proof with auto with datatypes.
    intros.
    destruct msg...
    + destruct st1.
      simpl in H.
      subst...
    + destruct st1.
      simpl in H.
      subst...
  Qed.

End Make.
