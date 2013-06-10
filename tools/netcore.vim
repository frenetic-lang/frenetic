" Language: NetCore
" Maintainer: Frenetic Lang Team
" Latest Revision: 07 June 2013

if exists("b:current_syntax")
  finish
endif

syn match NCName '[a-zA-Z0-9_]*'
syn keyword NCBuiltIn nat monitorPolicy
syn match NCNumber '\d*' 
syn match NCMac '\([a-fA-F0-9][a-fA-F0-9]:\)\{5}[a-fA-F0-9][a-fA-F0-9]'
syn match NCIPNumber '\(\d\{1,3}\.\)\{1,3}\d\{1,3}' 
syn region NCBlockCmd start="begin" end="end"
syn keyword NCLet let in nextgroup=NCName skipwhite
syn keyword NCKeyword if then else
syn keyword NCMatch inPort publicIP all fwd <none> filter switch vlan dlTyp dlDst dlSrc srcIP dstIP tcpSrcPort tcpDstPort frameType arp ip pass drop
syn keyword NCOperator "," "(" ")" "{" "}" "!" "*" "=" "->" "&&" "||" ";" "|" "+"
syn region NCComment start="(\*" end="\*)" 


let b:current_syntax = "netcore"

hi def link NCName          Function 
hi def link NCBlockCmd      Statement
hi def link NCLet           Statement 
hi def link NCKeyword       Conditional 
hi def link NCMAC	    Number 
hi def link NCNumber        Number 
hi def link NCIPNumber      Number 
hi def link NCMatch         Function 
hi def link NCOperator      Operator
hi def link NCBuiltIn       Function 
hi def link NCComment       Comment
