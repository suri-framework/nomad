open Riot

module Telemetry_ = struct
  type Telemetry.event += Request_received of { req : Http.Request.t }
    [@@unboxed]

  let request_received req = Telemetry.emit (Request_received { req })
end

module Http1 = struct
  let until char data =
    let rec go char data len =
      match%bitstring data with
      | {| curr : 8 : string; rest : -1 : bitstring |} ->
          if String.equal curr char then (len, rest) else go char rest (len + 8)
    in
    let len, rest = go char data 0 in
    let captured = Bitstring.subbitstring data 0 len in
    (captured, rest)

  let rec parse data =
    let data = IO.Buffer.to_string data |> Bitstring.bitstring_of_string in
    match do_parse data with exception _ -> `more | req -> `ok req

  and do_parse data =
    let meth, rest = parse_method data in
    let path, rest = parse_path rest in
    let version, rest = parse_protocol_version rest in
    let headers, _body = parse_headers rest [] in
    let req = Http.Request.make ~meth ~version ~headers path in
    req

  and parse_method data =
    let meth, rest = until " " data in
    let meth = Bitstring.string_of_bitstring meth in
    (Http.Method.of_string meth, rest)

  and parse_path data =
    let path, rest = until " " data in
    (Bitstring.string_of_bitstring path, rest)

  and parse_protocol_version : Bitstring.t -> Http.Version.t * Bitstring.t =
    function%bitstring
    | {| "HTTP/1.0" : 8*8 : string; rest : -1 : bitstring |} -> (`HTTP_1_0, rest)
    | {| "HTTP/1.1" : 8*8 : string; rest : -1 : bitstring |} -> (`HTTP_1_1, rest)

  and parse_headers data acc =
    let header, rest = parse_header data in
    let acc = header :: acc in
    match%bitstring rest with
    | {| "\r\n" : 2*8 : string ; rest : -1 : bitstring |} ->
        (Http.Header.of_list acc, rest)
    | {| "\n" : 8 : string ; rest : -1 : bitstring |} ->
        (Http.Header.of_list acc, rest)
    | {| _ |} -> parse_headers rest acc

  and parse_header data =
    let clean s =
      String.(s |> Bitstring.string_of_bitstring |> lowercase_ascii |> trim)
    in
    let h, rest = until ":" data in
    let v, rest = until "\n" rest in
    let h = clean h in
    let v = clean v in
    ((h, v), rest)

  let to_string (res : Http.Response.t) body =
    let buf = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer buf in
    Format.fprintf fmt "%a %a\r\n%s%s%!" Http.Version.pp res.version
      Http.Status.pp res.status
      (Http.Header.to_string res.headers)
      body;
    IO.Buffer.of_string (Buffer.contents buf)
end

module Atacama_handler = struct
  open Atacama.Handler
  include Atacama.Handler.Default

  let http1 () = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.request

  type state = {
    parser : Httpaf.Request.t Angstrom.Buffered.state;
    handler : Atacama.Socket.t -> Http.Request.t -> unit;
  }

  let run_handler conn state req =
    let Httpaf.Request.{ meth; target; version; headers } = req in
    let version =
      version |> Httpaf.Version.to_string |> Http.Version.of_string
    in
    let headers = headers |> Httpaf.Headers.to_list |> Http.Header.of_list in
    let req =
      Http.Request.make ~meth:(meth :> Http.Method.t) ~version ~headers target
    in
    state.handler conn req

  let handle_data data conn state =
    Logger.debug (fun f -> f "Parsing data %S" (IO.Buffer.to_string data));
    match state.parser with
    | Angstrom.Buffered.Partial partial -> (
        Logger.debug (fun f -> f "partial");
        let cs = IO.Buffer.as_cstruct data in
        let parser = partial (`Bigstring (Cstruct.to_bigarray cs)) in
        match parser with
        | Angstrom.Buffered.Done (_, req) ->
            run_handler conn state req;
            Close state
        | _ ->
            Logger.debug (fun f -> f "continue");
            Continue { state with parser })
    | Angstrom.Buffered.Done (_, req) ->
        Logger.debug (fun f -> f "done");
        run_handler conn state req;
        Close state
    | Angstrom.Buffered.Fail _ -> Close state

  let handle_close _conn _state =
    Logger.debug (fun f -> f "closing connection");
    ()
end
