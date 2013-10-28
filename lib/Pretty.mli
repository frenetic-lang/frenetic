open Types

val format_policy : Format.formatter -> policy -> unit

val format_header : Format.formatter -> header -> unit

val header_to_string : header -> string

val value_to_string : header_val -> string
  
val string_of_policy : policy -> string
