(* Write tests in independent modules, then just include them here to run them *)
open Test_Frenetic_Bits
open Test_Frenetic_Fdd
open Test_Frenetic_GroupTable0x04
open Test_Frenetic_NetKAT
open Test_Frenetic_NetKAT_Json
open Test_Frenetic_NetKAT_Lexer
open Test_Frenetic_NetKAT_Compiler
open Test_Frenetic_NetKAT_Parser
open Test_Frenetic_NetKAT_Pretty
open Test_Frenetic_Network
open Test_Frenetic_Packet
open Test_Frenetic_OpenFlow
open Test_Frenetic_OpenFlow0x01
open Test_Frenetic_OpenFlow_Header
open Test_Frenetic_NetKAT_SDN_Json
open Test_Frenetic_Util
open Test_Frenetic_Vlr

let _ = 
  Ppx_inline_test_lib.Runtime.summarize ()
