[@@@warning "-8"]

open Riot

let main () =
  Logger.set_log_level (Some Info);
  let (Ok _) = Logger.start () in
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
    Logger.debug (fun f -> f "res:\n%s" (Bigstringaf.to_string res));
    let bytes = Atacama.Socket.send conn res |> Result.get_ok in
    Logger.info (fun f -> f "wrote %d bytes" bytes);
    ()
  in

  let (Ok pid) =
    Atacama.start_link ~port:2112
      (module Nomad.Atacama_handler)
      { buffer = Bigstringaf.empty; handler }
  in
  wait_pids [ pid ]

let () = Riot.run @@ main
