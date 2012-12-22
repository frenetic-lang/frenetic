open Printf
open Word
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef
open NetCore

let policy = PoAtom (PrAll, [])

module Make (Platform : PLATFORM) = struct
  module Controller = MakeNetCoreController (Platform)

  let start () = Controller.start_controller policy

end
