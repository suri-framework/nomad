open Riot

module Echo_server = struct
  type args = unit
  type state = int


  let init (_args : args) : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
      `ok 1

  let handle_frame frame _conn _state : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
    Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    `push ([ frame ], _state)

  (* val handle_message : *)
  (*   Message.t -> state -> (state, [> `Unknown_opcode of int ]) handle_result *)
  let handle_message _message _state : (state, [> `Unknown_opcode of int]) Trail.Sock.handle_result =
      `ok 2
end

module Test : Application.Intf = struct

  let start () =
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

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
