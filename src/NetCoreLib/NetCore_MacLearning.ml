open Printf
open Packet
open OpenFlow0x01
open NetCore_Types.Internal
open NetCore_Action.Output
open NetCore_Pattern

module Learning = struct
    
  type int48 = int64 

  (** The objective is to learn the port on which a host is connected on all
      switches. Word48.t is the type of MAC-addresses. This hashtable stores
      switch * host * port tuples. The reason it is keyed-by (switch * host)
      is to acount for host-mobility. When a host moves to a different port 
      on a switch, updating its entry removes the old entry too. *)
  let learned_hosts : (switchId * int48, portId) Hashtbl.t = 
    Hashtbl.create 100

  (** Create a predicate that matches all unknown 
      switch * port * host * port tuples. *)
  let make_unknown_predicate () = 
    PrNot
      (Hashtbl.fold 
        (fun (sw,eth) pt pol -> 
          PrOr (PrAnd (PrAnd (PrOnSwitch sw, PrHdr (inPort (Physical pt))),
                       PrHdr (dlSrc eth)), pol))
        learned_hosts
        PrNone)

  (** We're using the Lwt library to create streams. Policy is a stream
      we can read. The push function sends a value into the policy 
      stream. *)
  let (policy, push) = Lwt_stream.create ()

  (** Create a policy that directs all unknown packets to the controller. These
      packets are sent to the [learn_host] function below. *)
  let rec make_learning_policy () = 
    PoSeq (PoFilter (make_unknown_predicate ()),
           PoAction (controller learn_host))

  (** Stores a new switch * host * port tuple in the table, creates a
      new learning policy, and pushes that policy to the stream. *)
  and learn_host sw pt pk  =
    match pt with
      | Physical pt ->
        begin
          begin
            printf "[MacLearning] at switch %Ld host %s at port %d\n%!"
	            sw (string_of_mac pk.dlSrc) pt;
            if Hashtbl.mem learned_hosts (sw, pk.dlSrc) then
              printf "[MacLearning.ml] at switch %Ld, host %s at port %d (moved)\n%!"
                sw (string_of_mac pk.dlSrc) pt
            else
              printf "[MacLearning.ml] at switch %Ld, host %s at port %d\n%!"
                sw (string_of_mac pk.dlSrc) pt
          end;
          Hashtbl.replace learned_hosts (sw, pk.dlSrc) pt;
          push (Some (make_learning_policy ()));
          drop
        end
      | _ -> drop
          
  
  (** The initial value of the policy is to receives packets from all hosts. *)

  let init = 
    PoSeq (PoFilter (PrHdr all), PoAction (controller learn_host))

  let _ = push (Some init)

end 

module Routing = struct

  let known_hosts () = 
    Hashtbl.fold 
      (fun (sw,dst) _ hosts -> PrOr (PrAnd (PrOnSwitch sw, PrHdr (dlDst dst)),
                                     hosts))
      Learning.learned_hosts
      PrNone

  (** Maps over all tuples, (sw, pt, mac) in [learned_hosts], and
      writes the rule:
      
      Switch = sw && DstMac = mac ==> To pt
      
      Sends traffic for unknown destinations to ToAll ports. *)
  let make_routing_policy () = 
    Hashtbl.fold
      (fun (sw, dst) pt pol ->
        PoUnion
          (PoSeq (PoFilter (PrAnd (PrOnSwitch sw, PrHdr (dlDst dst))),
                  PoAction (forward pt)),
           pol))
      Learning.learned_hosts
      (PoSeq (PoFilter (PrNot (known_hosts ())), PoAction to_all))

  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let policy = Lwt_stream.map (fun learning_pol ->
    PoUnion (learning_pol, make_routing_policy ()))
    Learning.policy
end


