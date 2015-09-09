(* Write tests in independent modules, then just include them here to run them *)
open Test_Frenetic_Bits
open Test_Frenetic_NetKAT
open Test_Frenetic_NetKAT_Local_Compiler
open Test_Frenetic_NetKAT_Pretty
open Test_Frenetic_Network
open Test_Frenetic_Packet
open Test_Frenetic_OpenFlow
open Test_Frenetic_OpenFlow0x01

Pa_ounit_lib.Runtime.summarize ()
