open Core.Std
open Frenetic_NetKAT_Optimize
open Frenetic_NetKAT

type typ = Src | Dst

let parse_ip (typ : typ) (str : string): pred =
  match String.split str ~on:'/' with
  | [addr; mask] ->
    let addr = Ipaddr.V4.(to_int32 (of_string_exn addr)) in
    let mask = Int32.of_string mask in
    (match typ with
     | Src -> Test (IP4Src (addr, mask))
     | Dst -> Test (IP4Dst (addr, mask)))
  | _ -> failwith ("invalid IP address/mask: " ^ str)

let parse_port (typ : typ) (str : string) : pred =
  match String.split str ~on:':' |>
    List.map ~f:String.strip |>
    List.map ~f:Int.of_string with
  | [0; 65535] -> True
  | [lo; _] ->
    (match typ with
       | Src -> Test (TCPSrcPort lo)
       | Dst -> Test (TCPDstPort lo))
  | _ -> assert false
(* OpenFlow doesn't let us succinctly represent port ranges. We can enumerate,
   but then policies become huge. *)
(*   | [lo; hi] ->
      let f x = match typ with
        | Src -> Test (TCPSrcPort x)
        | Dst -> Test (TCPDstPort x) in
    if hi - lo > 65536 - (hi - lo) then
      List.range ~stop:`inclusive lo hi |> List.map ~f |> mk_big_or
    else
      let ports = List.range 0 lo @ List.range (hi + 1) 65536 in
      Neg (List.map ~f ports |> mk_big_or)
 *)

let parse_proto (str : string) : pred = match String.split str ~on:'/' with
 | [ _; "0x00" ] -> True
 | [ proto; "0xFF" ] ->
   And (Test (IPProto (Int.of_string proto)), Test (EthType 0x800))
 | _ -> failwith ("bad proto " ^ str)

let parse_classbench ~(filename : string) : pred =
  let lines = In_channel.read_lines filename in
  let f (line : string) = match String.split line ~on:'\t' with
    | [ srcIp; dstIp; srcPt; dstPt; proto; _; _ ]
    | [ srcIp; dstIp; srcPt; dstPt; proto; _ ] -> (* ignoring flags *)
      let srcIp = String.drop_prefix srcIp 1 in (* leading @ *)
      let srcIp = parse_ip Src srcIp in
      let dstIp = parse_ip Dst dstIp in
      let srcPt = parse_port Src srcPt in
      let dstPt = parse_port Dst dstPt in
      let proto = parse_proto proto in
      mk_big_and [srcIp; dstIp; srcPt; dstPt; proto]
    | _ -> failwith "cannot parse line" in
  mk_big_or (List.map lines ~f)
