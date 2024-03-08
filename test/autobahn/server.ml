
module Echo_server = struct
  type args = unit
  type state = int


  let init (_args : args) : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
      `ok 1

  let handle_frame frame _conn _state : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
    Riot.Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    `push ([ frame ], _state)

  (* val handle_message : *)
  (*   Message.t -> state -> (state, [> `Unknown_opcode of int ]) handle_result *)
  let handle_message _message _state : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
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

    let handler = Nomad.trail [ws_echo] in

    Nomad.start_link ~port:2112 ~handler ()
end

module Autobahn  = struct

    let get_cwd () =
        try
            Unix.getcwd ()
        with
        | Unix.Unix_error (err, _, _) ->
                Printf.eprintf "Could not get current directory: %s\n" (Unix.error_message err);
                exit 1



    let launch server_pid () = 
        Riot.link server_pid;

        let open Unix in
        
        let cwd = get_cwd () in
        
        let config_volume = Filename.concat cwd "/test/autobahn/fuzzingclient.json:/fuzzingclient.json" in
        let reports_volume = Filename.concat cwd "/_build/reports:/reports" in
        
        let cmd = "docker" in
        let args = [|
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
			 "ws://0.0.0.0:2112"
        |] in
        
        let pid = Unix.create_process cmd args stdin stdout stderr in
        
        let _, status = Unix.waitpid [] pid in
        
        match status with
        | _ -> 
                (* the docker image always exits with 0, so if exits stop the server *)
                Riot.Logger.info (fun f -> f "autobahn exited");
                Riot.(exit server_pid Normal);
                ()


    let start server_pid =
        let open Riot in
        let pid = spawn @@ launch server_pid in
        Ok pid
end

let () = Riot.run @@ fun () ->
    let (let*) = Result.bind in

    let launch () =
        let* logger_pid = Riot.Logger.start () in
        let* server_pid = Test.start () in
        (* if i dont wait a little bit autobahn gets weird and hangs forever *)
        Unix.sleep 2;
        let* autobahn_pid = Autobahn.start server_pid in
        Riot.Logger.info (fun f -> f "started autobahn pid: %a" Riot.Pid.pp autobahn_pid);

        Riot.wait_pids [ logger_pid; server_pid; autobahn_pid ];

        Ok () in

    match launch () with
    | Ok () -> ()
    | Error _ -> Riot.shutdown ()
