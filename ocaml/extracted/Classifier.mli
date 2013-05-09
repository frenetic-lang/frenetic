open Datatypes
open List0
open NetworkPacket
open Peano

type 'a coq_Classifier = (Pattern.pattern * 'a) list

val scan : 'a1 -> 'a1 coq_Classifier -> portId -> packet -> 'a1

val inter_entry :
  ('a1 -> 'a1 -> 'a2) -> 'a1 coq_Classifier -> (Pattern.pattern * 'a1) ->
  (Pattern.Pattern.pat * 'a2) list

val inter :
  ('a1 -> 'a1 -> 'a2) -> 'a1 coq_Classifier -> 'a1 coq_Classifier ->
  (Pattern.Pattern.pat * 'a2) list

val union :
  ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Classifier -> 'a1 coq_Classifier ->
  (Pattern.Pattern.pat * 'a1) list

val elim_shadowed_helper :
  'a1 coq_Classifier -> 'a1 coq_Classifier -> 'a1 coq_Classifier

val elim_shadowed : 'a1 coq_Classifier -> 'a1 coq_Classifier

val prioritize :
  int -> 'a1 coq_Classifier -> ((int * Pattern.pattern) * 'a1) list

val sequence_atom :
  ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> Pattern.pattern -> 'a1
  -> Pattern.pattern -> 'a1 -> Pattern.Pattern.pat * 'a1

val sequence_helper :
  ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> Pattern.pattern -> 'a1
  -> 'a1 coq_Classifier -> (Pattern.Pattern.pat * 'a1) list

val sequence :
  ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Classifier ->
  'a1 coq_Classifier -> (Pattern.Pattern.pat * 'a1) list

