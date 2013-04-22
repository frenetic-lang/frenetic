open Unix
open OpenFlow0x01Types
open NetCoreSyntax
open NetCore

let policy = Pol (All, [ToAll])

module Make (Platform : OpenFlow0x01.Sig.PLATFORM) = struct
  module Controller = Make (Platform)

  let start () = 
    let (stream, push) = Lwt_stream.create () in
    push (Some policy);
    Controller.start_controller stream

end
