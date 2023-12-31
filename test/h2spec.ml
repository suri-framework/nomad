[@@@warning "-8"]

open Riot
open Trail

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Debug);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let hello_world conn = conn |> Conn.send_response `OK ~body:"hello world" in

    let handler = Nomad.trail [ hello_world ] in

    Nomad.start_link ~port:2112 ~handler ()
end

module H2Spec = struct
  let name = "h2spec"

  let start () =
    let pid =
      spawn (fun () ->
          sleep 1.;
          let args =
            {|docker run --network host summerwind/h2spec -h "0.0.0.0" -p 2112 -tk --strict|}
            |> String.split_on_char ' ' |> Array.of_list
          in
          let proc =
            Unix.create_process "docker" args Unix.stdin Unix.stdout Unix.stderr
          in
          let rec wait () =
            match Unix.waitpid [ Unix.WNOHANG ] proc with
            | proc_id, Unix.WEXITED exit_code when proc_id = proc -> exit_code
            | _ ->
                yield ();
                wait ()
          in
          let exit_code = wait () in
          Stdlib.exit exit_code)
    in
    Ok pid
end

let () = Riot.start ~apps:[ (module Logger); (module Test); (module H2Spec) ] ()
