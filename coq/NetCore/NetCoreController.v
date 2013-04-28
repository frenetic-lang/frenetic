Set Implicit Arguments.

Require Import Common.Types.
Require Import Common.Monad.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Pattern.Pattern.
Require Import Classifier.Classifier.
Require Import NetCore.NetCoreEval.
Require Import NetCore.NetCoreCompiler.
Require Import OpenFlow.ControllerInterface.

Local Open Scope list_scope.

Section Prioritize.

  (** TODO(arjun): deal with priority underflowing 16 bits *)
  Fixpoint prio_rec {A : Type} (prio : Word16.t) (lst : Classifier A) :=
    match lst with
      | nil => nil
      | (pat, act) :: rest => 
        (prio, pat, act) :: (prio_rec (Word16.pred prio) rest)
    end.

  Definition prioritize {A : Type} (lst : Classifier A) :=
    prio_rec Word16.max_value lst.

End Prioritize.

Section PacketIn.

  Definition packetIn_to_in (sw : switchId) (pktIn : packetIn) :=
    InPkt sw (packetInPort pktIn) (packetInPacket pktIn)
      (packetInBufferId pktIn).

End PacketIn.

Section ToFlowMod.
  
  Definition maybe_openflow0x01_modification {A : Type} (newVal : option A)
             (mkModify : A -> OpenFlow0x01Types.action) : actionSequence :=
    match newVal with
      | None => nil
      | Some v => [mkModify v]
    end.
  
  Definition modification_to_openflow0x01 (mods : modification) : actionSequence :=
    match mods with
      | Modification dlSrc dlDst dlVlan dlVlanPcp 
                     nwSrc nwDst nwTos
                     tpSrc tpDst =>
        maybe_openflow0x01_modification dlSrc SetDlSrc ++
        maybe_openflow0x01_modification dlDst SetDlDst ++
        maybe_openflow0x01_modification (withVlanNone dlVlan) SetDlVlan ++
        maybe_openflow0x01_modification dlVlanPcp SetDlVlanPcp ++
        maybe_openflow0x01_modification nwSrc SetNwSrc ++
        maybe_openflow0x01_modification nwDst SetNwDst ++
        maybe_openflow0x01_modification nwTos SetNwTos ++
        maybe_openflow0x01_modification tpSrc SetTpSrc ++
        maybe_openflow0x01_modification tpDst SetTpDst
    end.

  (** TODO(arjun): This is *wrong*. You can't trivially compile modifications like this.
   It only works b/c this controller (which we extract for dynamic policies) is unverified. *)
  Definition translate_action (in_port : option portId) (act : act) : actionSequence :=
    match act with
      | Forward mods (PhysicalPort pp) =>
        modification_to_openflow0x01 mods ++
        [match in_port with
           | None => Output (PhysicalPort pp)
           | Some pp' => match Word16.eq_dec pp' pp with
                           | left _ => Output InPort
                           | right _ => Output (PhysicalPort pp)
                         end
         end]
      | Forward mods p => modification_to_openflow0x01 mods ++ [Output p]
      | ActGetPkt x => [Output (Controller Word16.max_value)]
    end.

  Definition to_flow_mod (prio : priority) (pat : pattern) (act : list act)
             (isfls : Pattern.is_empty pat = false) :=
    let ofMatch := Pattern.to_match isfls in
    FlowMod AddFlow
            ofMatch
            prio
            (concat_map (translate_action (matchInPort ofMatch)) act)
            Word64.zero
            Permanent
            Permanent
            false
            None
            None
            false.

  Definition flow_mods_of_classifier lst :=
    List.fold_right
      (fun (ppa : priority * pattern * list act)
           (lst : list flowMod) => 
         match ppa with
           | (prio,pat,act) => 
             (match (Pattern.is_empty pat) as b
                    return (Pattern.is_empty pat = b -> list flowMod) with
                | true => fun _ => lst
                | false => fun H => (to_flow_mod prio act H) :: lst
              end) eq_refl
         end)
      nil
      (prioritize lst).

  Definition delete_all_flows := 
    FlowMod DeleteFlow
            (* This should make reasoning easier, since we have so many
               theorems about patterns. *)
            (Pattern.to_match Pattern.all_is_not_empty)
            Word16.zero
            nil
            Word64.zero
            Permanent
            Permanent
            false
            None
            None
            false.
End ToFlowMod.

Record ncstate := State {
  policy : pol;
  switches : list switchId
}.

Module Type NETCORE_MONAD <: CONTROLLER_MONAD.

  Include MONAD.

  (** These functions are from CONTROLLER_MONAD, with the [state]
     parameter specialized to [ncstate]. *)
  Definition state := ncstate.
  Parameter get : m state.
  Parameter put : state -> m unit.
  Parameter send : switchId -> xid -> message -> m unit.
  Parameter recv : m event.
  Parameter forever : m unit -> m unit.

  (** These functions are NetCore-specific. *)
  Parameter handle_get_packet : id -> switchId -> portId -> packet -> m unit.

End NETCORE_MONAD.

Module Make (Import Monad : NETCORE_MONAD).

  Local Notation "x <- M ; K" := (bind M (fun x => K)).

  Fixpoint sequence (lst : list (m unit)) : m unit := 
    match lst with
      | nil => ret tt
      | cmd :: lst' =>
        bind cmd (fun _ => sequence lst')
    end.
  
  Definition config_commands (pol: pol) (swId : switchId) :=
    sequence
      (List.map
         (fun fm => send swId Word32.zero (FlowModMsg fm))
         (delete_all_flows 
            :: (flow_mods_of_classifier (compile_opt pol swId)))).

  Definition set_policy (pol : pol) := 
    st <- get;
    let switch_list := switches st in
    _ <- put (State pol switch_list);
    _ <- sequence (List.map (config_commands pol) switch_list);
    ret tt.

  Definition handle_switch_disconnected (swId : switchId) :=
    st <- get;
    let switch_list := 
        List.filter 
          (fun swId' => match Word64.eq_dec swId swId' with
                          | left _ => false
                          | right  _ => true
                        end)
          (switches st) in
    _ <- put (State (policy st) switch_list);
    ret tt.

  (** I'm assuming disconnected and connected are interleaved. OCaml
      should provide that guarantee. *)
  Definition handle_switch_connected (swId : switchId) :=
    st <- get;
    _ <- put (State (policy st) (swId :: (switches st)));
    _ <- config_commands (policy st) swId;
    ret tt.

  Definition send_output (out : output) := 
    match out with
      | OutNothing => ret tt
      | OutPkt swId pp pkt bufOrBytes =>
          send swId Word32.zero
               (PacketOutMsg (PacketOut bufOrBytes None [Output pp]))
      | OutGetPkt x switchId portId packet => 
        handle_get_packet x switchId portId packet
    end.
  
  Definition handle_packet_in (swId : switchId) (pk : packetIn) := 
    st <- get;
    let outs := classify (policy st) (packetIn_to_in swId pk) in
    sequence (List.map send_output outs).

  Definition handle_event evt := 
    match evt with
      | SwitchDisconnected swId => handle_switch_disconnected swId
      | SwitchConnected swId => handle_switch_connected swId
      | SwitchMessage swId xid (PacketInMsg pktIn) => 
        handle_packet_in swId pktIn
      | SwitchMessage swId xid msg => ret tt
    end.

  Definition main := forever (evt <- recv; handle_event evt).

End Make.
