open Printf
open Packet
open NetCore_Types
open NetCore_Action.Output

type int48 = int64 

let make sw inside outside = 

  (** On switch sw, 
        (1) forward all ip packets from port inside to port outside
        (2) drop all ip packets from outside to inside initially
        (3) when inside sends to dstIP X, allow srcIP X to send back to inside
        (4) drop all non-ip packets
      *)

  (** policies is a stream we can read. 
      push sends a value into the policy stream. *)
  let (policies, push) = Lwt_stream.create () in

  let is_ip_packet = Hdr (dlTyp 0x800) in

  (** extend pol with new flow to allow 
      (1) packets with ipSrc ip from outside to inside 
      (2) packets with ipDst ip from inside to outside *) 
  let new_flow ip pol =
    ITE (And(Hdr (inPort (Physical outside)), 
             And(is_ip_packet, Hdr (ipSrc ip))), 
         Action (forward inside), 
         ITE (And(Hdr (inPort (Physical inside)), 
                  And(is_ip_packet, Hdr (ipDst ip))), 
         Action (forward outside), 
         pol)) in

  let on_switch pol = ITE (OnSwitch sw, pol, Action []) in

  (* the current policy; set to dummy value; reinitialized to init below *)
  let current = ref (Action []) in

  (* extend current policy to allow flows from outside in with
     srcIP = nwDst pk *)
  let allow_flow pk  =
    let new_dest = nwDst pk in
    printf "[FW] at switch %Ld allows %s \n%!" sw (string_of_nwAddr new_dest);
    let new_pol = new_flow new_dest (!current) in
    current := new_pol;
    push (Some (on_switch new_pol));
    forward outside in

  (** An initial policy that 
        (1) sends all ip packets from inside to outside to the controller
        (2) drops all others *)
  let init = 
    ITE (And(Hdr (inPort (Physical inside)), is_ip_packet), 
      Action (controller (fun sw port pk -> allow_flow pk)), 
      Action []) in

  current := init;
  push (Some (on_switch init));
  NetCore_Stream.from_stream init policies
