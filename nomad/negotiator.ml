open Riot

let ( let* ) = Result.bind

let alpn_protocol conn =
  match Atacama.Connection.negotiated_protocol conn with
  | Some "http/1.1" -> `http1
  | Some "h2" -> `http2
  | _ -> `no_match

let sniff_wire conn =
  let* data = Atacama.Connection.receive ~limit:24 conn in
  match IO.Buffer.to_string data with
  | "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" -> Ok `http2
  | data -> Ok (`no_match data)

let negotiated_protocol ~enabled_protocols conn =
  let enabled proto = List.mem proto enabled_protocols in
  let* wire = sniff_wire conn in
  let alpn = alpn_protocol conn in
  match (alpn, wire) with
  | (`http2 | `no_match), `http2 when enabled `http2 ->
      Ok (Session.make (module Protocol.Http2) None)
  | (`http1 | `no_match), `no_match data when enabled `http1 ->
      Ok (Session.make (module Protocol.Http1) (Some data))
  | _ -> Error `No_protocol_matched
