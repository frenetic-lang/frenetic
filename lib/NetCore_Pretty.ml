open Format
open NetCore_Types
open NetCore_Pattern

let to_string_exact = NetCore_Wildcard.to_string_exact


let string_of_port = function
  | Physical pid -> (string_of_portId pid)
  | Queue (pid, qid) -> (string_of_portId pid)  ^ " " ^ (Int32.to_string qid)
  | All -> "all"
  | Here -> "pass"

module Format = struct
  
  let format_list fmt sep lst =
    let rec loop fmt lst = match lst with
      | [x; y] -> fprintf fmt "@[%s%s@]@[%s@]" x sep y
      | [x] -> fprintf fmt "@[%s@]" x
      | [] ->  ()
      | x :: lst' -> fprintf fmt "@[%s%s@]@[%a@]" x sep loop lst' in
    fprintf fmt "@[%a@]" loop lst

  let pat fmt pat =
    if is_all pat then
      fprintf fmt "*"
    else if is_empty pat then
      fprintf fmt "none"
    else 
      format_list fmt " && "
        (List.filter (fun x -> not (x = "")) 
              [ to_string_exact Packet.string_of_mac "dlSrc" pat.ptrnDlSrc;
          to_string_exact Packet.string_of_mac "dlDst" pat.ptrnDlDst;
          to_string_exact Packet.string_of_dlTyp "dlTyp" pat.ptrnDlTyp;
          to_string_exact Packet.string_of_dlVlan "vlan" pat.ptrnDlVlan;
          to_string_exact string_of_int "dlVlanPcp" pat.ptrnDlVlanPcp;
          to_string_exact Packet.string_of_ip "srcIP" pat.ptrnNwSrc;
          to_string_exact Packet.string_of_ip "dstIP" pat.ptrnNwDst;
          to_string_exact Packet.string_of_nwProto "nwProto" pat.ptrnNwProto;
          to_string_exact string_of_int "nwTos" pat.ptrnNwTos;
          to_string_exact string_of_int "tcpSrcPort" pat.ptrnTpSrc;
          to_string_exact string_of_int "tcpDstPort" pat.ptrnTpDst;
          to_string_exact string_of_port "inPort" pat.ptrnInPort ])

  let match_modify pr lbl fmt mm = match mm with
    | None -> ()
    | Some (old, new_) -> fprintf fmt "@[@[%s@ %s@,->@,%s@];@ @]"
      lbl (pr old) (pr new_)

  let output fmt (out : output) : unit =
    fprintf fmt "@[%a%a%a%a%a%a%a%a%a%s@]"
      (match_modify Packet.string_of_dlAddr "dlSrc") out.outDlSrc
      (match_modify Packet.string_of_dlAddr "dlDst") out.outDlDst
      (match_modify Packet.string_of_dlVlan "dlVlan") out.outDlVlan
      (match_modify Packet.string_of_dlVlanPcp "dlVlanPcp") out.outDlVlanPcp
      (match_modify Packet.string_of_ip "nwSrc") out.outNwSrc
      (match_modify Packet.string_of_ip "nwDst") out.outNwDst
      (match_modify Packet.string_of_nwTos "nwTos") out.outNwTos
      (match_modify string_of_int "tpSrc") out.outTpSrc
      (match_modify string_of_int "tpDst") out.outTpDst
      (string_of_port out.outPort)

  let action fmt action : unit = match action with
    | SwitchAction o -> output fmt o
    | ControllerAction _ -> fprintf fmt "controller"
    | ControllerQuery (time, f) -> fprintf fmt "controllerQuery %f" time

  let rec action_list fmt lst = match lst with
    | [] -> fprintf fmt "drop"
    | [x] -> action fmt x
    | x :: lst' -> fprintf fmt "@[%a@ | %a@]" action x action_list lst'

  let rec action_choice_list fmt lst = match lst with
    | [] -> fprintf fmt "drop"
    | [x] -> action_list fmt x
    | x :: lst' -> fprintf fmt "@[ (@[%a@]) <|> (@[%a@])]" action_list x action_choice_list lst'

  let rec pred fmt p = match p with
    | And (p1, p2) -> fprintf fmt "@[%a@ && %a@]" orpred p1 pred p2
    | _ -> orpred fmt p

  and orpred fmt p = match p with
    | Or (p1, p2) -> fprintf fmt "@[%a@ || %a@]" apred p1 pred p2
    | _ -> apred fmt p

  and apred fmt p = match p with 
    (* pat does create a single box *)
    | Hdr ptrn -> fprintf fmt "@[%a@]" pat ptrn 
    | OnSwitch sw -> fprintf fmt "@[switch = %Ld@]" sw
    | Not p' -> fprintf fmt "@[!%a@]" apred p'
    | Everything -> fprintf fmt "@[*@]"
    | Nothing -> fprintf fmt "@[none@]" 
    (* TODO(arjun): concrete syntax is "<none>", don't know how to escape *)
    | Or _
    | And _ -> fprintf fmt "@[(%a)@]" pred p

  let rec pol fmt p = match p with
    | Seq (p1, p2) -> fprintf fmt "@[@[%a;@ @]%a@]" cpol p1 seq_pol_list p2
    | Union (p1, p2) -> fprintf fmt "@[%a@ +@ %a@]" cpol p1 par_pol_list p2
    | _ -> cpol fmt p

  and seq_pol_list fmt p = match p with
    | Seq (p1, p2) -> fprintf fmt "@[@[%a;@]%a@]" cpol p1 seq_pol_list p2
    | _ -> cpol fmt p

  and par_pol_list fmt p = match p with
    | Union (p1, p2) -> fprintf fmt "@[@[%a+@]%a@]" cpol p1 par_pol_list p2
    | _ -> cpol fmt p

  and cpol fmt p = match p with
    | ITE (pr, then_pol, else_pol) ->
            fprintf fmt "@[if@ %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
        pred pr cpol then_pol cpol else_pol
    | _ -> apol fmt p

  and apol fmt p = match p with
    | HandleSwitchEvent _ -> fprintf fmt "@[handleSwitchEvent _@]"
    | Action a -> fprintf fmt "@[%a@]" action_list a
    | ActionChoice a -> fprintf fmt "@[%a@]" action_choice_list a
    | Filter pr -> fprintf fmt "@[filter %a@]" pred pr
    | Union _
    | Seq _
    | ITE _ -> fprintf fmt "@[(%a)@]" pol p

end

let format_pol = Format.pol

let format_pred = Format.pred

let string_of_mk formatter x =
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let string_of_pred = string_of_mk Format.pred

let string_of_pol = string_of_mk Format.pol

let string_of_action = string_of_mk Format.action_list

let string_of_pattern = string_of_mk Format.pat

let string_of_value = function 
  | Pkt (sid, port, pkt, pay) ->
    Printf.sprintf "(%Ld, %s, %s, _)" 
      sid (string_of_port port) (Packet.to_string pkt)
