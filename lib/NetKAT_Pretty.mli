open NetKAT_Types

val format_policy : Format.formatter -> policy -> unit

val string_of_policy : policy -> string

val string_of_pred : pred -> string

val pretty_assoc : policy -> policy
