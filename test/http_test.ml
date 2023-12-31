[@@@warning "-8"]

open Riot
open Trail

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Debug);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let hello_world (conn: Conn.t) = 
      Logger.debug (fun f -> f "%a" Request.pp conn.req);
      conn |> Conn.send_response `OK ~body:"hello world" in

    let handler = Nomad.trail [ hello_world ] in

    Nomad.start_link ~port:2112 ~handler ()
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
