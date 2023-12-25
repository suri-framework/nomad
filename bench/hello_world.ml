[@@@warning "-8"]

open Riot

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Error);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let handler conn req =
      Logger.debug (fun f -> f "req: %a" Http.Request.pp req);
      let status = `OK in
      let body = "hello world" in
      let headers =
        Http.Header.of_list
          [
            ("content-length", body |> String.length |> string_of_int);
            ("connection", "close");
          ]
      in

      let res = Http.Response.make ~version:req.version ~status ~headers () in
      let res = Nomad.Http1.to_string res body in
      Logger.debug (fun f -> f "res:\n%s" (IO.Buffer.to_string res));
      let bytes = Atacama.Socket.send conn res |> Result.get_ok in
      Logger.debug (fun f -> f "wrote %d bytes" bytes);
      ()
    in

    Atacama.start_link ~port:2112
      (module Nomad.Atacama_handler)
      { parser = Nomad.Atacama_handler.http1 (); handler }
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
