open Platform 
open Unix
open MessagesDef
open NetCoreSyntax
open NetCore


let policy = Pol (All, [ToAll])

module Make (Platform : PLATFORM) = struct
  module Controller = Make (Platform)

  let start () = 
    let (stream, push) = Lwt_stream.create () in
    push (Some policy);
    Controller.start_controller stream

end
