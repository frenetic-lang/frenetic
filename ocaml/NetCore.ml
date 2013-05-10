module Make = NetCore_Controller.Make
module Syntax = NetCore_Syntax

module NetCoreOpenFlow = Make (OpenFlow0x01.Platform)

let start_controller port policy_stream =
  OpenFlow0x01.Platform.init_with_port port;
  Lwt_main.run (NetCoreOpenFlow.start_controller policy_stream)

(*
module Modules = struct

  module Learning = NetCore_MacLearning.Learning
  module Routing = NetCore_MacLearning.Routing

end
*)
