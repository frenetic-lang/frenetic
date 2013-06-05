
Exercise 3: Traffic Monitoring
==============================

- Compose the firewall and repeater with a third application: Web
  traffic monitor (count the total number of packets sent to and from
  port 80).

### The Monitoring Function

- Use the following template:

```ocaml
open Ox
open OxPlatform
open OpenFlow0x01
module MyApplication : OXMODULE = struct

  include OxDefaults
  
  let num_http_packets = ref 0

  let switch_connected (sw : switchId) : unit = 
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let is_http_packet (pk : Packet.packet) : bool = 
    (* [FILL IN HERE]: write this predicate *)

  let packet_in_ex2 (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    (* [FILL IN HERE]: the packet_in function from exercise 2 *)

  (* [FILL IN HERE] You can use the packet_in function from OxTutorial2. *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    packet_in_ex2 sw xid pktIn;
    if is_http_packet (Payload.parse pktIn.PacketIn.payload) then
      begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
      end

end

module Controller = Make (MyApplication)
```

#### Testing 

- Same test sequence as above. But, when sending HTTP traffic, the counter
  should be incremented.

- Run a Web server on port 8080. Note that connectivity works, but the
  counter is not incremented.
