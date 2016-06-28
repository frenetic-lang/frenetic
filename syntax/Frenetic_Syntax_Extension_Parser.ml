
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
                  <:expr< Int64.of_string  $`str:n$  >> 
    
# 44
    | n = INT64 -> 
# 44
                    <:expr< Int64.of_string  $`str:n$  >> 
    
# 45
     | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 46
  ]];

  nk_int32: [[
      n = INT -> 
# 49
                  <:expr< Int32.of_string  $`str:n$  >> 
    
# 50
    | n = INT32 -> 
# 50
                    <:expr< Int32.of_string  $`str:n$  >> 
    
# 51
     | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 52
  ]];

  nk_int: [[
      n = INT -> 
# 55
                  <:expr< Int.of_string  $`str:n$  >> 
    
# 56
     | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 57
  ]];

  nk_ipv4: [[
      n = IP4ADDR ->
        
# 61
         <:expr< Ipaddr.V4.(to_int32 (of_string_exn  $`str:n$ )) >> 
     
# 62
      | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 63
  ]];

  nk_loc: [[
      switch = nk_int64; "@"; port = nk_int64 ->
        
# 67
         <:expr< ( $switch$ , $port$ ) >> 
  
# 68
  ]];

  nk_string_constant: [[
      sc = STRING_CONSTANT -> 
# 71
                               <:expr<  $`str:sc$  >> 
  
# 72
  ]];

  nk_pred_atom: [[
      "("; a = nk_pred; ")" ->
      a
    | "begin"; a = nk_pred; "end" ->
      a
    | "true" ->
      
# 80
       <:expr< True >> 
    
# 81
    | "false" ->
      
# 82
       <:expr< False >> 
    
# 83
    | "switch"; "="; sw = nk_int64 ->
      
# 84
       <:expr< Test (Switch  $sw$ ) >> 
    
# 85
    | "port"; "="; n = nk_int32 ->
      
# 86
       <:expr< Test (Location (Physical  $n$ )) >> 
    
# 87
    | "vswitch"; "="; sw = nk_int64 ->
      
# 88
       <:expr< (Test (VSwitch  $sw$ )) >> 
    
# 89
    | "vport"; "="; n = nk_int64 ->
      
# 90
       <:expr< Test (VPort  $n$ ) >> 
    
# 91
    | "vfabric"; "="; vfab = nk_int64 ->
      
# 92
       <:expr< Test (VFabric  $vfab$ ) >> 
    
# 93
    | "vlanId"; "="; n = nk_int ->
      
# 94
       <:expr< Test (Vlan  $n$ ) >> 
    
# 95
    | "vlanPcp"; "="; n = nk_int ->
      
# 96
       <:expr< Test (VlanPcp  $n$ ) >> 
    
# 97
    | "ethTyp"; "="; n = nk_int ->
      
# 98
       <:expr< Test (EthType  $n$ ) >> 
    
# 99
    | "ipProto"; "="; n = nk_int ->
      
# 100
       <:expr< Test (IPProto  $n$ ) >> 
    
# 101
    | "tcpSrcPort"; "="; n = nk_int ->
      
# 102
       <:expr< Test (TCPSrcPort  $n$ ) >> 
    
# 103
    | "tcpDstPort"; "="; n = nk_int ->
      
# 104
       <:expr< Test (TCPDstPort  $n$ ) >> 
    
# 105
    | "ethSrc"; "="; n = nk_int64 ->
      
# 106
       <:expr< Test (EthSrc  $n$ ) >> 
    
# 107
    | "ethDst"; "="; n = nk_int64 ->
      
# 108
       <:expr< Test (EthDst  $n$ ) >> 
    
# 109
    | "ip4Src"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      
# 110
       <:expr< Test (IP4Src ( $n$ ,  $m$ )) >> 
    
# 111
    | "ip4Src"; "="; n = nk_ipv4 ->
      
# 112
       <:expr< Test (IP4Src ( $n$ , 32l)) >> 
    
# 113
    | "ip4Dst"; "="; n = nk_ipv4; "/"; m = nk_int32 ->
      
# 114
       <:expr< Test (IP4Dst ( $n$ ,  $m$ )) >> 
    
# 115
    | "ip4Dst"; "="; n = nk_ipv4 ->
      
# 116
       <:expr< Test (IP4Dst ( $n$ , 32l)) >> 
    
# 117
     | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 118
  ]];

  nk_pred_not : [[
      a = nk_pred_atom -> a
    | "not"; a = nk_pred_not ->
      
# 123
       <:expr< Neg  $a$  >> 
  
# 124
  ]];

  nk_pred_and : [[
      a = nk_pred_not -> a
    | a = nk_pred_and; "and"; b = nk_pred_not ->
      
# 129
       <:expr< And ( $a$ ,  $b$ ) >> 
  
# 130
  ]];

  nk_pred_or : [[
      a = nk_pred_and -> a
    | a = nk_pred_or; "or"; b = nk_pred_and ->
      
# 135
       <:expr< Or ( $a$ ,  $b$ ) >> 
  
# 136
  ]];

  nk_pred: [[
      a = nk_pred_or -> a
  ]];

  nk_pkt_dest: [[
      n = nk_int32 -> 
# 143
                       <:expr< Physical n >> 
    
# 144
    | "query"; "("; q = nk_string_constant; ")" -> 
# 144
                                                    <:expr< Query q >> 
    
# 145
    | "pipe"; "("; p = nk_string_constant; ")" -> 
# 145
                                                   <:expr< Pipe p >> 
  
# 146
  ]];

  nk_pol_atom: [[
      "("; p = nk_pol; ")" ->
      p
    | "begin"; p = nk_pol; "end" ->
      p
    | "id" ->
      
# 154
       <:expr< id >> 
    
# 155
    | "drop" ->
      
# 156
       <:expr< drop >> 
    
# 157
    | "filter"; a = nk_pred ->
      
# 158
       <:expr< Filter  $a$  >> 
    
# 159
    | "switch"; ":="; sw = nk_int64 ->
      
# 160
       <:expr< Mod (Switch  $sw$ ) >> 
    
# 161
    | "port"; ":="; l = nk_pkt_dest ->
      
# 162
       <:expr< Mod (Location l) >> 
    
# 163
    | "vswitch"; ":="; sw = nk_int64 ->
      
# 164
       <:expr< Mod (VSwitch  $sw$ ) >> 
    
# 165
    | "vport"; ":="; n = nk_int64 ->
      
# 166
       <:expr< Mod (VPort  $n$ ) >> 
    
# 167
    | "vfabric"; ":="; vfab = nk_int64 ->
      
# 168
       <:expr< Mod (VFabric  $vfab$ ) >> 
    
# 169
    | "ethSrc"; ":="; n = nk_int64 ->
      
# 170
       <:expr< Mod (EthSrc  $n$ ) >> 
    
# 171
    | "ethDst"; ":="; n = nk_int64 ->
      
# 172
       <:expr< Mod (EthDst  $n$ ) >> 
    
# 173
    | "ethTyp"; ":="; n = nk_int ->
      
# 174
       <:expr< Mod (EthType  $n$ ) >> 
    
# 175
    | "vlanId"; ":="; n = nk_int ->
      
# 176
       <:expr< Mod (Vlan  $n$ ) >> 
    
# 177
    | "vlanPcp"; ":="; n = nk_int ->
      
# 178
       <:expr< Mod (VlanPcp  $n$ ) >> 
    
# 179
    | "ip4Src"; ":="; n = nk_ipv4 ->
      
# 180
       <:expr< Mod (IP4Src( $n$ , 32l)) >> 
    
# 181
    | "ip4Dst"; ":="; n = nk_ipv4 ->
      
# 182
       <:expr< Mod (IP4Dst( $n$ , 32l)) >> 
    
# 183
    | "ipProto"; ":="; n = nk_int ->
      
# 184
       <:expr< Mod (IPProto  $n$ ) >> 
    
# 185
    | "tcpSrcPort"; ":="; n = nk_int ->
      
# 186
       <:expr< Mod (TCPSrcPort  $n$ ) >> 
    
# 187
    | "tcpDstPort"; ":="; n = nk_int ->
      
# 188
       <:expr< Mod (TCPDstPort  $n$ ) >> 
    
# 189
    | loc1 = nk_loc; "=>"; loc2 = nk_loc -> 
# 189
                                             <:expr< 
      let (sw1, pt1) =  $loc1$  in
      let (sw2, pt2) =  $loc2$  in
      let pt1 = Int64.to_int32_exn pt1 in
      let pt2 = Int64.to_int32_exn pt2 in
      Link (sw1, pt1, sw2, pt2) >> 
    
# 195
    | loc1 = nk_loc; "=>>"; loc2 = nk_loc -> 
# 195
                                              <:expr< 
      let (sw1, pt1) =  $loc1$  in
      let (sw2, pt2) =  $loc2$  in
       <:expr< VLink (sw1, pt1, sw2, pt2) >>  >> 
    
# 199
     | `ANTIQUOT s -> Syntax.AntiquotSyntax.parse_expr _loc s 
  
# 200
  ]];

  nk_pol_star : [[
      p = nk_pol_atom -> p
    | p = nk_pol_star; "*" ->
      
# 205
       <:expr< Star  $p$  >> 
  
# 206
  ]];

  nk_pol_seq : [[
      p = nk_pol_star -> p
    | p = nk_pol_seq; ";"; q = nk_pol_star ->
      
# 211
       <:expr< Seq ( $p$ ,  $q$ ) >> 
  
# 212
  ]];

  nk_pol_union : [[
      p = nk_pol_seq -> p
    | p = nk_pol_union; "|"; q = nk_pol_seq ->
      
# 217
       <:expr< Union ( $p$ ,  $q$ ) >> 
  
# 218
  ]];

  nk_pol_cond : [[
      p = nk_pol_union -> p
    | "if"; a = nk_pred;
      "then"; p = nk_pol_cond;
      "else"; q = nk_pol_cond ->
      
# 225
       <:expr< Union(Seq(Filter  $a$ ,  $p$ ), Seq(Filter (Neg  $a$ ),  $q$ )) >> 
  
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

# 4 "syntax/Frenetic_Syntax_Extension_Parser.cppo.ml"
let parse_netkat_pol loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = policy_of_string ~loc s in
  Camlp4_config.antiquotations := q;
  result

let parse_netkat_pred loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = pred_of_string ~loc s in
  Camlp4_config.antiquotations := q;
  result

let () =
  let module Q = Syntax.Quotation in
  Q.add "netkat" Q.DynAst.expr_tag parse_netkat_pol;
  Q.add "nkpred" Q.DynAst.expr_tag parse_netkat_pred;
  Q.default := "netkat";
