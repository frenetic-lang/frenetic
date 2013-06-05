open Printf
open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Action.Output

type int48 = int64 

let make () = 

  (** The objective is to learn the port on which a host is connected on all
      switches. Word48.t is the type of MAC-addresses. This hashtable stores
      switch * host * port tuples. The reason it is keyed-by (switch * host)
      is to acount for host-mobility. When a host moves to a different port 
      on a switch, updating its entry removes the old entry too. *)
  let learned_hosts : (switchId * int48, portId) Hashtbl.t = 
    Hashtbl.create 100 in

  (** Create a predicate that matches all unknown 
      switch * port * host * port tuples. *)
  let make_unknown_predicate () = 
    PrNot
      (Hashtbl.fold 
        (fun (sw,eth) pt pol -> 
          PrOr (PrAnd (PrAnd (PrOnSwitch sw, PrHdr (inPort (Physical pt))),
                       PrHdr (dlSrc eth)), pol))
        learned_hosts
        PrNone) in

  (** We're using the Lwt library to create streams. Policy is a stream
      we can read. The push function sends a value into the policy 
      stream. *)
  let (policy, push) = Lwt_stream.create () in

  (** Create a policy that directs all unknown packets to the controller. These
      packets are sent to the [learn_host] function below. *)
  let rec make_learning_policy () = 
    (* PoSeq (PoFilter (make_unknown_predicate ()), *)
    (*        PoAction (controller learn_host)) *)
    PoITE (make_unknown_predicate (),PoAction (controller learn_host), PoAction [])


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
              printf "[Macml] at switch %Ld, host %s at port %d (moved)\n%!"
                sw (string_of_mac pk.dlSrc) pt
            else
              printf "[Macml] at switch %Ld, host %s at port %d\n%!"
                sw (string_of_mac pk.dlSrc) pt
          end;
          Hashtbl.replace learned_hosts (sw, pk.dlSrc) pt;
          push (Some (make_learning_policy ()));
          drop
        end
      | _ -> drop in
          
  
  (** The initial value of the policy is to receives packets from all hosts. *)

  let init = 
    (* PoSeq (PoFilter (PrHdr all), PoAction (controller learn_host)) in *)
    PoITE (PrHdr all, PoAction (controller learn_host), PoAction []) in

  let known_hosts () = 
    Hashtbl.fold 
      (fun (sw,dst) _ hosts -> PrOr (PrAnd (PrOnSwitch sw, PrHdr (dlDst dst)),
                                     hosts))
      learned_hosts
      PrNone in

  (** Maps over all tuples, (sw, pt, mac) in [learned_hosts], and
      writes the rule:
      
      Switch = sw && DstMac = mac ==> To pt
      
      Sends traffic for unknown destinations to ToAll ports. *)
  let make_routing_policy () = 
    Hashtbl.fold
      (fun (sw, dst) pt pol ->
        PoUnion
          (PoITE (PrAnd (PrOnSwitch sw, PrHdr (dlDst dst)),
                  PoAction (forward pt), PoAction []),
           pol))
      learned_hosts
      (PoITE (PrNot (known_hosts ()), PoAction to_all, PoAction [])) in

  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let policy = Lwt_stream.map (fun learning_pol ->
    PoUnion (learning_pol, make_routing_policy ()))
    policy in

  push (Some init);
  (init, policy)
