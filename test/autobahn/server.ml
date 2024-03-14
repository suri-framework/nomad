open Riot

module Echo_server = struct
  type args = unit
  type state = int

  let init (_args : args) :
      (state, [> `Unknown_opcode of int ]) Trail.Sock.handle_result =
    `ok 1

  let handle_frame frame _conn _state :
      (state, [> `Unknown_opcode of int ]) Trail.Sock.handle_result =
    Riot.Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    `push ([ frame ], _state)

  (* val handle_message : *)
  (*   Message.t -> state -> (state, [> `Unknown_opcode of int ]) handle_result *)
  let handle_message _message _state :
      (state, [> `Unknown_opcode of int ]) Trail.Sock.handle_result =
    `ok 2
end

module Test : Riot.Application.Intf = struct
  let start () =
    let open Riot in
    Logger.set_log_level (Some Debug);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let ws_echo (conn : Trail.Conn.t) =
      let handler = Trail.Sock.make (module Echo_server) () in
      let upgrade_opts = Trail.Sock.{ do_upgrade = true } in
      conn |> Trail.Conn.upgrade (`websocket (upgrade_opts, handler))
    in

    let handler = Nomad.trail [ ws_echo ] in

    Nomad.start_link ~port:2112 ~handler ()
end

module Autobahn = struct
  let spawn_docker args =
    let path =
      match Sys.getenv_opt "PATH" with
      | None -> []
      | exception Not_found -> []
      | Some s -> String.split_on_char ':' s
    in

    let find_prog prog =
      let rec search = function
        | [] -> None
        | x :: xs ->
            let prog = Filename.concat x prog in
            if Sys.file_exists prog then Some prog else search xs
      in
      search path
    in

    match find_prog "docker" with
    | None -> failwith "Failed to find docker executable in PATH"
    | Some prog ->
        Spawn.spawn ~prog ~argv:args ~stdin:Unix.stdin ~stdout:Unix.stdout
          ~stderr:Unix.stderr ()

  let init () =
    let cwd = Unix.getcwd () in

    let config_volume =
      Filename.concat cwd
        "/test/autobahn/fuzzingclient.json:/fuzzingclient.json"
    in
    let reports_volume = Filename.concat cwd "/_build/reports:/reports" in
    let args =
      [
        "docker";
        "run";
        "--rm";
        "-v";
        config_volume;
        "-v";
        reports_volume;
        "--name";
        "nomad";
        "--net=host";
        "crossbario/autobahn-testsuite";
        "wstest";
        "--mode";
        "fuzzingclient";
        "-w";
        "ws://0.0.0.0:2112";
      ]
    in

    let _ = spawn_docker args in

    match receive () with _ -> failwith "Should have never received a message"

  let start () =
    let pid = spawn init in
    Ok pid
end

let () =
  Riot.start ~apps:[ (module Riot.Logger); (module Test); (module Autobahn) ] ()
