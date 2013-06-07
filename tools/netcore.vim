" Vim syntax file
" Language: NetCore
" Maintainer: Frenetic Lang Team
" Latest Revision: 07 June 2013

if exists("b:current_syntax")
  finish
endif

syn match NCFunction '[a-zA-Z0-9_]*'
syn region NCComment start="(\*" end="\*)"
syn match NCNumber '\d+' contained display
syn keyword NCBlockCmd let in begin end 
syn keyword NCKeyword if then else
syn keyword NCMatch inPort publicIP all fwd <none> filter switch vlan srcMAC dstMAC srcIP dstIP tcpSrcPort tcpDstPort frameType arp ip
syn keyword NCOperator "," "(" ")" "{" "}" "!" "*" "=" "->" "&&" "||" ";" "|" "+"


let b:current_syntax = "netcore"

hi def link NCFunction Function 
hi def link NCBlockCmd    Statement
hi def link NCKeyword     Conditional 
hi def link NCNumber      Number
hi def link NCMatch      Constant
hi def link NCOperator      Operator
hi def link NCComment     Comment
