open Platform
open Printf
open Word
open MessagesDef

module Make (P : PLATFORM) = struct

  let rec accept_thread () = 
    let features = P.accept_switch () in
    let sw_id = features.switch_id in
    printf "[controller] accepted switch %Ld\n%!" (Word64.to_int64 sw_id);
    accept_thread ()

  let start () = 
    accept_thread ()

end
