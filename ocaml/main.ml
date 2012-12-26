open Printf
open Word
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef


let _ = OpenFlowPlatform.init_with_port 6633
    
module Controller = Repeater.Make (OpenFlowPlatform)

let _ = Controller.start ()
