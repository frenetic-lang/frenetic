open Printf
open Word
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef
open NetCore

let policy = Pol (All, [ToAll])

module Make (Platform : PLATFORM) = struct
  module Controller = Make (Platform)

  let start () = Controller.start_controller policy

end
