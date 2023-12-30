let text = "hello world"

let handler (request, _flow, _addr) =
  match Http.Request.resource request with
  | _ -> Cohttp_eio.Server.text_response text

let () =
  let port = 2112 in
  Eio_main.run @@ fun env -> Cohttp_eio.Server.run ~domains:9 ~port env handler
