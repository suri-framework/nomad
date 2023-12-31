open Riot

module Echo_server = struct
  type args = unit
  type state = int

  let init conn _args = `continue (conn, 0)

  let handle_frame frame _conn _state =
    Logger.info (fun f -> f "handling frame: %a" Trail.Frame.pp frame);
    `push [ frame ]
end

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Debug);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let ws_echo (conn : Trail.Conn.t) =
      let handler = Trail.Sock.make (module Echo_server) () in
      let upgrade_opts = Trail.Sock.{ do_upgrade = true } in
      conn |> Trail.Conn.upgrade (`websocket (upgrade_opts, handler))
    in

    let handler = Nomad.trail [ Trail.logger ~level:Debug (); ws_echo ] in

    Nomad.start_link ~port:2112
      ~transport:(Atacama.Transport.ssl ~config)
      ~handler ()
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
