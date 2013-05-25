open NetCore_Types

val desugar : (unit -> int option) -> External.policy -> Internal.pol
