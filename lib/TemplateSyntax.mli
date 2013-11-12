open Types
exception Eval_error of string

type pos = Lexing.position

type id = string

type width = int


val switch_width  : int
val macaddr_width : int
val ipaddr_width  : int
val port_width    : int
val ethtyp_width  : int
val vlan_width    : int
val ipproto_width : int



type typ =
  | TPred
  | THdr of width
  | TInt of width
  | TPol
  | TFun of typ list * typ


type exp =
  | Id of pos * id
  | Let of pos * id * exp * exp
  | Fun of pos * id list * exp
  | App of pos * exp * exp list
  | If of pos * exp * exp * exp
  | Par of pos * exp * exp
  | Seq of pos * exp * exp
  | Mod of pos * exp * exp
  | Filter of pos * exp
  | Link of pos * exp * exp * exp * exp
  | True of pos 
  | False of pos
  | Test of pos * exp * exp
  | And of pos * exp * exp
  | Or of pos * exp * exp
  | Neg of pos * exp
  | Header of pos * header
  | HeaderVal of pos * header_val
  | TypeIs of pos * exp * typ

val eval : exp -> policy
