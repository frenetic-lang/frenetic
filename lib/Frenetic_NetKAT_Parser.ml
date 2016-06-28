
# 13 "lib/Frenetic_NetKAT_Parser.cppo.ml"
open Core.Std
open Camlp4.PreCast
module Gram = MakeGram(Frenetic_NetKAT_Lexer)
open Frenetic_NetKAT_Lexer
open Frenetic_NetKAT

let nk_pred_eoi = Gram.Entry.mk "nk_pred_eoi"
let nk_pred = Gram.Entry.mk "nk_pred"
let nk_pred_atom = Gram.Entry.mk "nk_pred_atom"
let nk_pred_not = Gram.Entry.mk "nk_pred_not"
let nk_pred_and = Gram.Entry.mk "nk_pred_and"
let nk_pred_or = Gram.Entry.mk "nk_pred_or"
let nk_pol_eoi = Gram.Entry.mk "nk_pol_eoi"
let nk_pol = Gram.Entry.mk "nk_pol"
let nk_pol_atom = Gram.Entry.mk "nk_pol_atom"
let nk_pol_seq = Gram.Entry.mk "nk_pol_seq"
let nk_pol_star = Gram.Entry.mk "nk_pol_star"
let nk_pol_union = Gram.Entry.mk "nk_pol_union"
let nk_pol_cond = Gram.Entry.mk "nk_pol_cond"
let nk_int64 = Gram.Entry.mk "nk_int64"
let nk_int32 = Gram.Entry.mk "nk_int32"
let nk_int = Gram.Entry.mk "nk_int"
let nk_ipv4 = Gram.Entry.mk "nk_ipv4"
let nk_loc = Gram.Entry.mk "nk_loc"
let nk_string_constant = Gram.Entry.mk "nk_string_constant"
let nk_pkt_dest = Gram.Entry.mk "nk_pkt_dest"

EXTEND Gram

  nk_int64: [[
      n = INT -> 
# 43
                  Int64.of_string  n  
    
# 44
    | n = INT64 -> 
# 44
                    Int64.of_string  n  
    
# 45
     
  
# 46
  ]];

  nk_int32: [[
      n = INT -> 
# 49
                  Int32.of_string  n  
    
# 50
    | n = INT32 -> 
# 50
                    Int32.of_string  n  
    
# 51
     
  
# 52
  ]];

  nk_int: [[
      n = INT -> 
# 55
                  Int.of_string  n  
    
# 56
     
  
# 57
  ]];

  nk_ipv4: [[
      n = IP4ADDR ->
        
# 61
         Ipaddr.V4.(to_int32 (of_string_exn  n )) 
     
# 62
      
  
# 63
  ]];

  nk_loc: [[
      switch = nk_int64; "@"; port = nk_int64 ->
        
# 67
         ( switch , port ) 
  
# 68
  ]];

  nk_string_constant: [[
      sc = STRING_CONSTANT -> 
# 71
                                sc  
  
# 72
  ]];

  nk_pred_atom: [[
      "("; a = nk_pred; ")" ->
      a
    | "begin"; a = nk_pred; "end" ->
      a
    | "true" ->
      
# 80
       True 
    
# 81
    | "false" ->
      
# 82
       False 
    
# 83
    | "switch"; "="; sw = nk_int64 ->
      
# 84
       Test (Switch  sw ) 
    
# 85
    | "port"; "="; n = nk_int32 ->
      
# 86
       Test (Location (Physical  n )) 
    
# 87
    | "vswitch"; "="; sw = nk_int64 ->
      
# 88
       (Test (VSwitch  sw )) 
    
# 89
    | "vport"; "="; n = nk_int64 ->
      
# 90
       Test (VPort  n ) 
    
# 91
    | "vfabric"; "="; vfab = nk_int64 ->
      
# 92
       Test (VFabric  vfab ) 
    
# 93
    | "vlanId"; "="; n = nk_int ->
      
# 94
       Test (Vlan  n ) 
    
# 95
    | "vlanPcp"; "="; n = nk_int ->
      
# 96
       Test (VlanPcp  n ) 
    
# 97
    | "ethTyp"; "="; n = nk_int ->
      
# 98
       Test (EthType  n ) 
    
# 99
    | "ipProto"; "="; n = nk_int ->
      
# 100
       Test (IPProto  n ) 
    
# 101
    | "tcpSrcPort"; "="; n = nk_int ->
      
# 102
       Test (TCPSrcPort  n ) 
    
# 103
    | "tcpDstPort"; "="; n = nk_int ->
      
# 104
       Test (TCPDstPort  n ) 
    
# 105
    | "ethSrc"; "="; n = nk_int64 ->
      
# 106
       Test (EthSrc  n ) 
    
# 107
    | "ethDst"; "="; n = nk_int64 ->
      
# 108
       Test (EthDst  n ) 
    
# 109
    | "ip4Src"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      
# 110
       Test (IP4Src ( n ,  m )) 
    
# 111
    | "ip4Src"; "="; n = nk_ipv4 ->
      
# 112
       Test (IP4Src ( n , 32l)) 
    
# 113
    | "ip4Dst"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      
# 114
       Test (IP4Dst ( n ,  m )) 
    
# 115
    | "ip4Dst"; "="; n = nk_ipv4 ->
      
# 116
       Test (IP4Dst ( n , 32l)) 
    
# 117
     
  
# 118
  ]];

  nk_pred_not : [[
      a = nk_pred_atom -> a
    | "not"; a = nk_pred_not ->
      
# 123
       Neg  a  
  
# 124
  ]];

  nk_pred_and : [[
      a = nk_pred_not -> a
    | a = nk_pred_and; "and"; b = nk_pred_not ->
      
# 129
       And ( a ,  b ) 
  
# 130
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> a
    | a = nk_pred_or; "or"; b = nk_pred_and ->
      
# 135
       Or ( a ,  b ) 
  
# 136
  ]];

  nk_pred: [[
      a = nk_pred_or -> a
  ]];

  nk_pkt_dest: [[
      n = nk_int32 -> 
# 143
                       Physical n 
    
# 144
    | "query"; "("; q = nk_string_constant; ")" -> 
# 144
                                                    Query q 
    
# 145
    | "pipe"; "("; p = nk_string_constant; ")" -> 
# 145
                                                   Pipe p 
  
# 146
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" ->
      p
    | "begin"; p = nk_pol; "end" ->
      p
    | "id" ->
      
# 154
       id 
    
# 155
    | "drop" ->
      
# 156
       drop 
    
# 157
    | "filter"; a = nk_pred ->
      
# 158
       Filter  a  
    
# 159
    | "switch"; ":="; sw = nk_int64 ->
      
# 160
       Mod (Switch  sw ) 
    
# 161
    | "port"; ":="; l = nk_pkt_dest ->
      
# 162
       Mod (Location l) 
    
# 163
    | "vswitch"; ":="; sw = nk_int64 ->
      
# 164
       Mod (VSwitch  sw ) 
    
# 165
    | "vport"; ":="; n = nk_int64 ->
      
# 166
       Mod (VPort  n ) 
    
# 167
    | "vfabric"; ":="; vfab = nk_int64 ->
      
# 168
       Mod (VFabric  vfab ) 
    
# 169
    | "ethSrc"; ":="; n = nk_int64 ->
      
# 170
       Mod (EthSrc  n ) 
    
# 171
    | "ethDst"; ":="; n = nk_int64 ->
      
# 172
       Mod (EthDst  n ) 
    
# 173
    | "ethTyp"; ":="; n = nk_int ->
      
# 174
       Mod (EthType  n ) 
    
# 175
    | "vlanId"; ":="; n = nk_int ->
      
# 176
       Mod (Vlan  n ) 
    
# 177
    | "vlanPcp"; ":="; n = nk_int ->
      
# 178
       Mod (VlanPcp  n ) 
    
# 179
    | "ip4Src"; ":="; n = nk_ipv4 ->
      
# 180
       Mod (IP4Src( n , 32l)) 
    
# 181
    | "ip4Dst"; ":="; n = nk_ipv4 ->
      
# 182
       Mod (IP4Dst( n , 32l)) 
    
# 183
    | "ipProto"; ":="; n = nk_int ->
      
# 184
       Mod (IPProto  n ) 
    
# 185
    | "tcpSrcPort"; ":="; n = nk_int ->
      
# 186
       Mod (TCPSrcPort  n ) 
    
# 187
    | "tcpDstPort"; ":="; n = nk_int ->
      
# 188
       Mod (TCPDstPort  n ) 
    
# 189
    | loc1 = nk_loc; "=>"; loc2 = nk_loc -> 
# 189
                                             
      let (sw1, pt1) =  loc1  in
      let (sw2, pt2) =  loc2  in
      let pt1 = Int64.to_int32_exn pt1 in
      let pt2 = Int64.to_int32_exn pt2 in
      Link (sw1, pt1, sw2, pt2) 
    
# 195
    | loc1 = nk_loc; "=>>"; loc2 = nk_loc -> 
# 195
                                              
      let (sw1, pt1) =  loc1  in
      let (sw2, pt2) =  loc2  in
       VLink (sw1, pt1, sw2, pt2)  
    
# 199
     
  
# 200
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> p
    | p = nk_pol_star; "*" ->
      
# 205
       Star  p  
  
# 206
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> p
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      
# 211
       Seq ( p ,  q ) 
  
# 212
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> p
    | p = nk_pol_union; "|"; q = nk_pol_seq ->
      
# 217
       Union ( p ,  q ) 
  
# 218
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> p
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      
# 225
       Union(Seq(Filter  a ,  p ), Seq(Filter (Neg  a ),  q )) 
  
# 226
  ]];

  nk_pol : [[ p = nk_pol_cond -> p ]];

  nk_pol_eoi: [[ x = nk_pol; `EOI -> x ]];
  nk_pred_eoi: [[ x = nk_pred; `EOI -> x ]];

END


let report loc e =
  failwith (Loc.to_string loc ^ ": " ^ Exn.to_string e)

let policy_of_stream ?(loc=(Loc.mk "<N/A>")) (s : char Stream.t) =
  try Gram.parse nk_pol_eoi loc s
  with Loc.Exc_located (loc, e) -> report loc e

let pred_of_stream ?(loc=(Loc.mk "<N/A>")) (s : char Stream.t) =
  try Gram.parse nk_pred_eoi loc s
  with Loc.Exc_located (loc, e) -> report loc e

let policy_of_string ?(loc=(Loc.mk "<N/A>")) (s : string) =
  policy_of_stream ~loc (Stream.of_string s)

let pred_of_string ?(loc=(Loc.mk "<N/A>")) (s : string) =
  pred_of_stream ~loc (Stream.of_string s)

let policy_of_file (file : string) =
  In_channel.with_file file ~f:(fun ch -> policy_of_stream ~loc:(Loc.mk file) (Stream.of_channel ch))

let pred_of_file (file : string) =
  In_channel.with_file file ~f:(fun ch -> pred_of_stream ~loc:(Loc.mk file) (Stream.of_channel ch))
