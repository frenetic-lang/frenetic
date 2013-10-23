module type HEADERS = sig
  type header
  type value
  type payload

  val switch : header
  val port : header
  val format_header : Format.formatter -> header -> unit
  val format_value : Format.formatter -> value -> unit
  val header_to_string : header -> string
  val value_to_string : value -> string
  val compare_header : header -> header -> int
end

module type S = sig

  type header
  type header_val
  type payload

  val switch : header
  val port : header

  type pred = 
    | True
    | False
    | Test of header * header_val
    | And of pred * pred
    | Or of pred * pred
    | Neg of pred

  type policy =
    | Filter of pred
    | Mod of header * header_val
    | Par of policy * policy
    | Choice of policy * policy
    | Seq of policy * policy
    | Star of policy
    | Link of header_val * header_val * header_val * header_val

  val id : policy

  val drop : policy

  module HeaderMap : Map.S
    with type key = header

  type header_val_map = header_val HeaderMap.t

  type packet = {
    headers : header_val_map;
    payload : payload
  }

  module PacketSet : Set.S
    with type elt = packet

  module PacketSetSet : Set.S
    with type elt = PacketSet.t

  val eval : packet -> policy -> PacketSetSet.t

  val format_policy : Format.formatter -> policy -> unit

  val string_of_policy : policy -> string

end

module Make (Headers : HEADERS) = struct

  type header = Headers.header
  type header_val = Headers.value
  type payload = Headers.payload

  let switch = Headers.switch
  let port = Headers.port

  type pred = 
    | True
    | False
    | Test of header * header_val
    | And of pred * pred
    | Or of pred * pred
    | Neg of pred

  type policy =
    | Filter of pred
    | Mod of header * header_val
    | Par of policy * policy
    | Choice of policy * policy
    | Seq of policy * policy
    | Star of policy
    | Link of header_val * header_val * header_val * header_val

  let id = Filter True

  let drop = Filter False

  module HeaderMap = Map.Make (struct
      type t = header
      let compare = Headers.compare_header
    end)

  type header_val_map = header_val HeaderMap.t

  type packet = {
    headers : header_val_map;
    payload : payload
  }

  module PacketSet = Set.Make (struct
      type t = packet

      (* First compare by headers, then payload. The payload comparison is a
         little questionable. However, this is safe to use in eval, since
         all output packets have the same payload as the input packet. *)
      let compare x y =
        let cmp = HeaderMap.compare Pervasives.compare x.headers y.headers in
        if cmp != 0 then
          cmp
        else
          Pervasives.compare x.payload y.payload
    end)

  module PacketSetSet = Set.Make(PacketSet)

  let rec eval_pred (pkt : packet) (pr : pred) : bool = match pr with
    | True -> true
    | False -> false
    | Test (h, v) -> HeaderMap.find h pkt.headers = v
    | And (pr1, pr2) -> eval_pred pkt pr1 && eval_pred pkt pr2
    | Or (pr1, pr2) -> eval_pred pkt pr1 || eval_pred pkt pr2
    | Neg pr1 -> not (eval_pred pkt pr1)

  let rec eval (pkt : packet) (pol : policy) : PacketSetSet.t = match pol with
    | Filter pr -> 
      if eval_pred pkt pr then 
        PacketSetSet.singleton (PacketSet.singleton pkt)
      else 
        PacketSetSet.empty
    | Mod (h, v) ->
      if HeaderMap.mem h pkt.headers then
        PacketSetSet.singleton (PacketSet.singleton { pkt with headers = HeaderMap.add h v pkt.headers })
      else
        raise Not_found (* for consistency with Test *)
    | Par (pol1, pol2) ->
      let cartesian_product s1 s2 =
        let f x y setset = PacketSetSet.add (PacketSet.union x y) setset in
        let g x setset = PacketSetSet.fold (f x) s2 setset in
        PacketSetSet.fold g s1 PacketSetSet.empty in
      cartesian_product (eval pkt pol1) (eval pkt pol2)
    | Seq (pol1, pol2) ->
      let f pkt' setset = PacketSetSet.union (eval pkt' pol2) setset in
      let g pktset = PacketSet.fold f pktset PacketSetSet.empty in
      let h pktset setset = PacketSetSet.union (g pktset) setset in
      PacketSetSet.fold h (eval pkt pol1) PacketSetSet.empty
    | Star pol -> raise Not_found (* MARCO: Can't figure this out :( *)
    (*let rec loop acc = 
      let f pkt' set = PacketSet.union (eval pkt' pol) set in 
      let acc' = PacketSet.fold f acc PacketSet.empty in 
      if PacketSet.equal acc acc' then acc else loop acc' in 
      loop (PacketSet.singleton pkt)*)
    | Choice (pol1, pol2) ->
      PacketSetSet.union (eval pkt pol1) (eval pkt pol2)

    | Link(sw,pt,sw',pt') -> 
      begin 
        try 
          if HeaderMap.find Headers.switch pkt.headers = sw && 
             HeaderMap.find Headers.port pkt.headers = pt then
            let pkt' = { pkt with headers = HeaderMap.add Headers.switch sw' (HeaderMap.add Headers.port pt' pkt.headers) } in 	
            PacketSetSet.singleton (PacketSet.singleton pkt')
          else
            PacketSetSet.empty
        with Not_found -> 
          raise Not_found
      end

  module Formatting = struct
    open Format

    (* The type of the immediately surrounding policy_context, which guides parenthesis-
       insertion. *)
    type predicate_context = OR_L | OR_R | AND_L | AND_R | NEG | PAREN_PR

    let rec pred (cxt : predicate_context) (fmt : formatter) (pr : pred) : unit = 
      match pr with
      | True -> 
        fprintf fmt "@[id@]"
      | False -> 
        fprintf fmt "@[drop@]"
      | (Test (h, v)) -> 
        fprintf fmt "@[%a = %a@]" Headers.format_header h 
          Headers.format_value v
      | Neg p' -> 
        begin match cxt with
          | PAREN_PR
          | NEG -> fprintf fmt "@[not %a@]" (pred NEG) p'
          | _ -> fprintf fmt "@[not@ @[(%a)@]@]" (pred PAREN_PR) p'
        end

      | And (p1, p2) -> 
        begin match cxt with
          | PAREN_PR
          | OR_L
          | OR_R
          | AND_L -> fprintf fmt "@[%a and %a@]" (pred AND_L) p1 (pred AND_R) p2
          | _ -> fprintf fmt "@[(@[%a and %a@])@]" (pred AND_L) p1 (pred AND_R) p2
        end

      | Or (p1, p2) -> 
        begin match cxt with
          | PAREN_PR
          | OR_L -> fprintf fmt "@[%a or %a@]" (pred OR_L) p1 (pred OR_R) p2
          | _ -> fprintf fmt "@[(@[%a or %a@])@]" (pred OR_L) p1 (pred OR_R) p2
        end

    type policy_context = SEQ_L | SEQ_R | PAR_L | PAR_R | CHOICE_L | CHOICE_R | STAR | PAREN

    let rec pol (cxt : policy_context) (fmt : formatter) (p : policy) : unit =
      match p with
      | Filter pr -> 
        (match pr with
         | True 
         | False -> pred PAREN_PR fmt pr
         | _ -> pp_print_string fmt "filter "; pred PAREN_PR fmt pr)

      | Mod (h, v) -> 
        fprintf fmt "@[%a := %a@]" Headers.format_header h
                                   Headers.format_value v
      | Star p' -> 
        begin match cxt with
          | PAREN 
          | STAR -> fprintf fmt "@[%a*@]" (pol STAR) p' 
          | _ -> fprintf fmt "@[@[(%a)*@]@]" (pol PAREN) p'
        end

      | Par (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR_L -> fprintf fmt "@[%a | %a@]" (pol PAR_L) p1 (pol PAR_R) p2
          | _ -> fprintf fmt "@[(@[%a | %a@])@]" (pol PAR_L) p1 (pol PAR_R) p2
        end

      | Seq (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR_L
          | PAR_R
          | SEQ_L -> fprintf fmt "@[%a ; %a@]" (pol SEQ_L) p1 (pol SEQ_R) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (pol SEQ_L) p1 (pol SEQ_R) p2
        end

      | Choice (p1, p2) -> (* MARCO: WHAT SYMBOL DO WE USE? *)
        begin match cxt with
          | PAREN
          | CHOICE_L -> fprintf fmt "@[%a + %a@]" (pol CHOICE_L) p1 (pol CHOICE_R) p2
          | _ -> fprintf fmt "@[(@[%a + %a@])@]" (pol CHOICE_L) p1 (pol CHOICE_R) p2
        end
      | Link (sw,pt,sw',pt') -> 
        fprintf fmt "@[%a@%a => %a@%a@]"
          Headers.format_value sw Headers.format_value pt
          Headers.format_value sw' Headers.format_value pt'
  end

  let format_policy = Formatting.pol Formatting.PAREN

  let string_of_policy = NetCore_Util.make_string_of format_policy
end
