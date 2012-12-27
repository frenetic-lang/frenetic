open Printf
open Word
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef
open NetCoreSemantics

let policy = PoAtom (PrAll, [Forward AllPorts])

module Make (Platform : PLATFORM) = struct
  module Controller = MakeNetCoreController (Platform) (EmptyHandlers)

  let start () = Controller.start_controller policy

end
