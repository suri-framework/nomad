[@@@warning "-8"]

open Riot

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Info);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let hello_world (conn : Trail.Conn.t) =
      conn |> Trail.Conn.send_response ~status:`OK ~body:"hello world"
    in

    let handler = Nomad.trail [ hello_world ] in

    Nomad.start_link ~port:2112 ~handler ()
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
