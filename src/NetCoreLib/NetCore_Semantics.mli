open Packet
open OpenFlow0x01
open NetCore_Types

val match_pred : pred -> switchId -> port -> packet -> bool

val eval_action : value -> action -> value list

(** Formally, a NetCore policy is interpreted as a function from packets to
bags of packets.  This function implements this interpretation.

However, because the packet ([val]) may be buffered on the switch, and thus not
entirely present at the controller, [eval pol val] produces an action [act]
that, when applied to [val] ([eval_action val act]), produces the output
packets.  

[act] is then returned to the switch as part of the [packetOut] message, along
with the buffer ID containing the original packet, and the switch unbuffers the
packet and applies [act] to it. 

[eval pol val] will invoke [ControllerAction] action atoms (i.e. it is
side-effecting).  [act] will only contain [SwitchAction] action atoms.  *)
val eval : pol -> value -> action

val classify : pol -> value -> value list

val handle_switch_events : switchEvent -> pol -> unit
