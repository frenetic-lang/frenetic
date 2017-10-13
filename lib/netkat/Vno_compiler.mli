(**
The "Virtual Network Object" (VNO) Compiler.

A VNO is a virtual network topology together with a mapping of the virtual
topology to the physical topology and a virtual policy that is to be executed
in the virtual network.

Given a physical network topology and any number of VNOs for that topology, this
compiler computes a single global program that implements all VNOs on the
physical network.
*)

open Syntax
open Local_compiler

type t
(** A physical network together with some VNOs *)

type vno =
  { vtopo: policy;
    ving: pred;
    veg: pred;
    vrel: pred;
    ving_pol: policy }


val create : topo:policy -> ing:pred -> eg:pred -> t

val add_vno :
     id:int
  -> vtopo:policy
  -> ving:pred
  -> veg:pred
  -> vrel:pred
  -> ?ving_pol:policy
  -> t
  -> t

val set_pol : id:int -> pol:policy -> t -> t

val compile : ?options:Local_compiler.compiler_options
           -> ?pc:Fdd.Field.t
           -> t
           -> FDD.t
(** [compile t] compiles the network [t] into an FDD. The pc field is used for
    internal bookkeeping and must *not* be accessed or written to by the virtual
    policies - it defauls to Vlan.
 *)
