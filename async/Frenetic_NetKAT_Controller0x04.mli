open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow0x04_Controller

module type Controller = sig
  val update_policy : policy -> unit Deferred.t
  val start : int -> unit
end

module Make : Controller

