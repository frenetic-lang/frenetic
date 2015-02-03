open OUnitHack
open NetKAT_Types
open NetKAT_LocalCompiler

TEST "clearing cache fails" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:`Empty (Filter b) in
  try
    seq fdd1 fdd2 != compile ~cache:`Keep (Filter (And (a, b)))
  with
    Not_found -> true

TEST "keeping cache works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:`Keep (Filter b) in
  seq fdd1 fdd2 = compile ~cache:`Keep (Filter (And (a, b)))

TEST "keeping reachable nodes in cache works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:(`Preserve fdd1) (Filter b) in
  seq fdd1 fdd2 = compile ~cache:`Keep (Filter (And (a, b)))
