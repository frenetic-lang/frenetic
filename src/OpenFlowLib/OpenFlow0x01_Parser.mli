
type msg_code

val msg_code_to_int : msg_code -> int

module Header : sig

  val ver : int

  type t = 
    { ver: int;
      typ: msg_code;
      len: int;
      xid: OpenFlow0x01.xid }
      
  val parse : Cstruct.t -> t 

  val to_string : t -> string

end

(* MJR: The rules on where to put the parser and pretty printer are
   unclear. Just sticking here to make things work *)

module PortStatus : sig

  val to_string : OpenFlow0x01.portStatus -> string

end

module Message : sig

    type t = OpenFlow0x01.message
	
    val parse : Header.t -> Cstruct.t -> (OpenFlow0x01.xid * t) option

    val msg_code_of_message : t -> msg_code

    val to_string : t -> string

    val sizeof_body : t -> int

    val blit_message : t -> Cstruct.t -> unit

    val marshal : OpenFlow0x01.xid -> t -> string 

  end

val sizeof_ofp_header : int
