module Semantics = Semantics.Make (SDN_Headers)

include Semantics

let string_of_vint = NetCore_Util.make_string_of VInt.format 

let string_of_header = SDN_Headers.header_to_string

