open Platform

module Make (P : PLATFORM) = struct

  let rec accept_thread () = 
    let sw_id = P.accept_switch () in
    accept_thread ()

  let _ = accept_thread ()

end
