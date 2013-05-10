open NetworkPacket
open OpenFlow0x01Types
open PatternSignatures
open Misc

module Make = 
 functor (Pattern_:PATTERN) ->
 struct 
  module Pattern = Pattern_
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t = bool
  
  type e = bool
  
  (** val atoms : t -> e list **)
  
  let atoms b =
    b :: []
  
  (** val drop : t **)
  
  let drop =
    false
  
  (** val pass : t **)
  
  let pass =
    true
  
  (** val apply_atom : e -> (port * packet) -> (port * packet) option **)
  
  let apply_atom b ptpk =
    if b then Some ptpk else None
  
  (** val apply_action : t -> (port * packet) -> (port * packet) list **)
  
  let apply_action action ptpk =
    filter_map (fun a -> apply_atom a ptpk) (atoms action)
  
  (** val par_action : t -> t -> t **)
  
  let par_action b1 b2 =
    (||) b1 b2
  
  (** val seq_action : t -> t -> t **)
  
  let seq_action b1 b2 =
    (&&) b1 b2
  
  (** val restrict_range : e -> pattern -> pattern **)
  
  let restrict_range b p =
    p
  
  (** val domain : e -> Pattern.t **)
  
  let domain b =
    Pattern.all
 end

