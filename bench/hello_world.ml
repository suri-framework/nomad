[@@@warning "-8"]

open Riot

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Info);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let handler conn req =
      Logger.debug (fun f -> f "req: %a" Trail.Request.pp req);
      let status = `OK in
      let body = IO.Buffer.of_string "hello world" in
      let headers =
        Http.Header.of_list
          [ ("content-length", body |> IO.Buffer.length |> string_of_int) ]
      in

      let res = Http.Response.make ~version:req.version ~status ~headers () in
      let res = Nomad.Protocol.Http1.to_string res in
      Logger.debug (fun f -> f "res:\n%s" (IO.Buffer.to_string res));
      let _bytes = Atacama.Connection.send conn res |> Result.get_ok in
      let bytes = Atacama.Connection.send conn body |> Result.get_ok in
      Logger.debug (fun f -> f "wrote %d bytes" bytes);

      Nomad.Handler.Close conn
    in

    Nomad.start_link ~port:2112 ~handler ()
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
