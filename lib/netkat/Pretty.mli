open Syntax

val format_pred : Format.formatter -> pred -> unit

val format_policy : Format.formatter -> policy -> unit

val string_of_policy : policy -> string

val string_of_pred : pred -> string

val pretty_assoc : policy -> policy
