open PolicyGenerator

let help args =
  match args with 
    | [ "run" ] -> 
      Format.printf "usage: katnetic run [local|classic|automaton] <filename> \n"
    | [ "dump" ] -> 
      Format.printf "usage: katnetic dump automaton [all|policies|flowtables] <filename> \n";
      Format.printf "usage: katnetic dump local [all|policies|flowtables] <number of switches> <filename> \n"
    | _ -> 
      Format.printf "%s" ("usage: katnetic <command> \n" ^
			  "  run    Compile and start the controller\n" ^ 
			  "  dump   Compile and dump flow table\n")

module Run = struct
  open LocalCompiler.RunTime

  let with_channel f chan =
    let exp = Parser.program Lexer.token (Lexing.from_channel chan) in
    Lwt_main.run (Controller.start f 6633 (NetKAT_Stream.constant exp))

  let with_file f filename =
    with_channel f (open_in filename)

  let local p =
    (fun sw -> to_table (compile sw p))

  let classic p =
    let p' = Dehop.dehop_policy p in
    local p'

  let automaton p =
    let open NetKAT_Automaton in
    let open Types in
    let i,m,_,e = dehopify p in
    let cache = SwitchMap.mapi (fun sw sw_p ->
      let sw_p' = Types.(Seq(Seq(i,sw_p),e)) in
      to_table (compile sw sw_p'))
    m in
    (fun sw -> SwitchMap.find sw cache)

  let main args =
    match args with
      | [filename]
      | ("local"     :: [filename]) -> with_file local filename
      | ("classic"   :: [filename]) -> with_file classic filename
      | ("automaton" :: [filename]) -> with_file automaton filename
      | _ -> help [ "run" ]
end

module Dump = struct
  open LocalCompiler.RunTime

  let with_channel f chan =
    f (Parser.program Lexer.token (Lexing.from_channel chan))

  let with_file f filename =
    with_channel f (open_in filename)

  module Local = struct

    let with_compile (sw : VInt.t) (p : Types.policy) =
      let _ = Printf.printf "Compiling switch %ld [size=%d]...%!"
        (VInt.get_int32 sw) (Semantics.size p) in
      let t1 = Unix.gettimeofday () in
      let i = compile sw p in
      let t2 = Unix.gettimeofday () in
      let t = to_table i in
      let t3 = Unix.gettimeofday () in
      let _ = Printf.printf "Done [ctime=%fs ttime=%fs tsize=%d]\n%!"
        (t2 -. t1) (t3 -. t2) (List.length t) in
      t

    let flowtable (sw : VInt.t) t =
      if List.length t > 0 then
        Format.printf "@[flowtable for switch %ld:\n%a@\n\n@]%!"
          (VInt.get_int32 sw)
          SDN_Types.format_flowTable t

    let policy p =
      Format.printf "@[%a\n\n@]%!" Pretty.format_policy p

    let local f sw_num p =
      (* NOTE(seliopou): This may not catch all ports, but it'll catch some of
       * 'em! Also, lol for loop.
       * *)
      for sw = 0 to sw_num do
        let vs = VInt.Int64 (Int64.of_int sw) in 
        let sw_p = Types.(Seq(Filter(Test(Switch,vs)), p)) in 
        let t = with_compile vs sw_p in
        f vs t
      done

    let all sw_num p =
      policy p;
      local flowtable sw_num p

    let stats sw_num p =
      local (fun x y -> ()) sw_num p

    let main args =
      match args with
        | (sw_num :: [filename])
        | ("all"        :: [sw_num; filename]) -> with_file (all (int_of_string sw_num)) filename
        | ("policies"   :: [sw_num; filename]) -> with_file policy filename
        | ("flowtables" :: [sw_num; filename]) -> with_file (local flowtable (int_of_string sw_num)) filename
        | ("stats"      :: [sw_num; filename]) -> with_file (stats (int_of_string sw_num)) filename
        | _ -> 
          print_endline "usage: katnetic dump local [all|policies|flowtables|stats] <number of switches> <filename>"
  end

  module Automaton = struct
    open NetKAT_Automaton

    let with_dehop f p =
      let _ = Printf.printf "Dehopify...%!" in
      let t1 = Unix.gettimeofday () in
      let i,m,_,e = dehopify p in
      let _ = Printf.printf "Done [size=%d time=%fs]\n%!" (Semantics.size p)
        (Unix.gettimeofday () -. t1) in
      SwitchMap.iter (fun sw pol0 ->
        let open Types in
        let pol0' = Seq(Seq(i,pol0),e) in
        f sw pol0')
      m

    let policy (sw : VInt.t) (p : Types.policy) : unit =
      Format.printf "@[policy for switch %ld:\n%!%a\n\n@]%!"
        (VInt.get_int32 sw)
        Pretty.format_policy p

    let flowtable (sw : VInt.t) (p : Types.policy) : unit =
      let t = Local.with_compile sw p in
      Local.flowtable sw t

    let all (sw : VInt.t) (p : Types.policy) : unit =
      policy sw p;
      flowtable sw p

    let stats (sw : VInt.t) (p : Types.policy) : unit =
      let _ = Local.with_compile sw p in
      ()

    let main args =
      match args with
        | [filename]
        | ("all"        :: [filename]) -> with_file (with_dehop all) filename
        | ("policies"   :: [filename]) -> with_file (with_dehop policy) filename
        | ("flowtables" :: [filename]) -> with_file (with_dehop flowtable) filename
        | ("stats"      :: [filename]) -> with_file (with_dehop stats) filename
        | _ -> 
          print_endline "usage: katnetic dump automaton [all|policies|flowtables|stats] <filename>"
  end

  module Classic = struct
    open Dehop

    module SwitchSet = Set.Make (struct
      type t = VInt.t
      let compare = Pervasives.compare
    end)

    let rec get_switches t =
      let open Types in match t with
        | Par(p,q) -> SwitchSet.union (get_switches p) (get_switches q)
        | Link(sw,_,sw',p) -> SwitchSet.add sw (SwitchSet.singleton sw')
        | _ -> SwitchSet.empty

    let with_dehop f p =
      let _ = Printf.printf "Dehopify...%!" in
      let t1 = Unix.gettimeofday () in
      let i,p,t,e = policy_to_dehopd_policy p in
      let switches = get_switches t in
      let _ = Printf.printf "Done [size=%d time=%fs]\n%!" (Semantics.size p)
        (Unix.gettimeofday () -. t1) in
      SwitchSet.iter (fun sw ->
        let open Types in
        let sw_f  = Filter(Test(Switch, sw)) in
        let p' = Seq(Seq(i,Seq(sw_f,p)),e) in
        f sw p')
      switches

    let policy (sw : VInt.t) (p : Types.policy) : unit =
      Format.printf "@[policy for switch %ld:\n%!%a\n\n@]%!"
        (VInt.get_int32 sw)
        Pretty.format_policy p

    let flowtable (sw : VInt.t) (p : Types.policy) : unit =
      let t = Local.with_compile sw p in
      Local.flowtable sw t

    let all (sw : VInt.t) (p : Types.policy) : unit =
      policy sw p;
      flowtable sw p

    let stats (sw : VInt.t) (p : Types.policy) : unit =
      let _ = Local.with_compile sw p in
      ()

    let main args =
      match args with
        | [filename]
        | ("all"        :: [filename]) -> with_file (with_dehop all) filename
        | ("policies"   :: [filename]) -> with_file (with_dehop policy) filename
        | ("flowtables" :: [filename]) -> with_file (with_dehop flowtable) filename
        | ("stats"      :: [filename]) -> with_file (with_dehop stats) filename
        | _ ->
          print_endline "usage: katnetic dump automaton [all|policies|flowtables|stats] <filename>"
  end

  let main args  =
    match args with
      | ("local"     :: args') -> Local.main args'
      | ("automaton" :: args') -> Automaton.main args'
      | ("classic" :: args') -> Classic.main args'
      | _ -> help [ "dump" ]

end

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run"  :: args) -> Run.main args
    | (_ :: "dump" :: args) -> Dump.main args
    | _ -> help []
