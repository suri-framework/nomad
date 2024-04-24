open Riot
open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

open Logger.Make (struct
  let namespace = [ "nomad"; "http1" ]
end)

module Parser = struct
  exception Bad_request
  exception Need_more_data
  exception Uri_too_long
  exception Header_fields_too_large

  open Logger.Make (struct
    let namespace = [ "nomad"; "http1"; "parser" ]
  end)

  let until ?(max_len = -1) char data =
    let rec go char data len =
      let left = Bitstring.bitstring_length data in
      if left = 0 then raise_notrace Need_more_data
      else if max_len > 0 && len > max_len then raise_notrace Uri_too_long
      else
        match%bitstring data with
        | {| curr : (String.length char * 8) : string; rest : -1 : bitstring |}
          ->
            if String.equal curr char then (len, rest)
            else
              let rest =
                Bitstring.(
                  concat
                    [
                      bitstring_of_string
                        (String.sub curr 1 (String.length curr - 1));
                      rest;
                    ])
              in
              go char rest (len + 8)
        | {| _ |} -> raise_notrace Need_more_data
    in
    let len, rest = go char data 0 in
    let captured = Bitstring.subbitstring data 0 len in
    (captured, rest)

  let rec parse ~(config : Config.t) data =
    let str = Bytestring.to_string data in
    let bit = str |> Bitstring.bitstring_of_string in
    match do_parse ~config bit with
    | exception Bad_request -> `bad_request
    | exception Uri_too_long -> `uri_too_long
    | exception Header_fields_too_large -> `header_fields_too_large
    | exception Need_more_data -> `more data
    | req ->
        trace (fun f ->
            f "parsed_request: %a -> preread_body=%d" Trail.Request.pp req
              (Bytestring.length req.buffer));
        `ok req

  and do_parse ~config data =
    let meth, rest = parse_method config.max_line_request_length data in
    let path, rest = parse_path config.max_line_request_length rest in
    let version, rest = parse_protocol_version rest in
    let headers, rest =
      parse_headers config.max_header_count config.max_header_length rest
    in
    trace (fun f -> f "creating request");
    let body = Bitstring.string_of_bitstring rest in
    let body = Bytestring.of_string body in
    Trail.Request.make ~body ~meth ~version ~headers path

  and parse_method max_len data =
    let meth, rest = until ~max_len " " data in
    let meth = Bitstring.string_of_bitstring meth in
    (Http.Method.of_string meth, rest)

  and parse_path max_len data =
    let path, rest = until ~max_len " " data in
    (Bitstring.string_of_bitstring path, rest)

  and parse_protocol_version data =
    match%bitstring data with
    | {| "HTTP/1.0" : 8*8 : string; rest : -1 : bitstring |} -> (`HTTP_1_0, rest)
    | {| "HTTP/1.1" : 8*8 : string; rest : -1 : bitstring |} -> (`HTTP_1_1, rest)
    | {| _ |} -> raise_notrace Bad_request

  and parse_headers max_count max_length data =
    match%bitstring data with
    (* we found the end of the headers *)
    | {| "\r\n\r\n" : 4 * 8 : string ; rest : -1 : bitstring |} ->
        trace (fun f -> f "found body");
        ([], rest)
    (* we found a beginning to the headers, but nothing more, so we need more data *)
    | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring|}
      when Bitstring.bitstring_length rest = 0 ->
        trace (fun f -> f "need more data");
        raise_notrace Need_more_data
    (* we found a beginning to the headers, so we can try to parse them *)
    | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring |} ->
        trace (fun f -> f "parsing headers");
        do_parse_headers max_count max_length rest []
    (* anything else is probably a bad request *)
    | {| _ |} -> raise_notrace Bad_request

  and header_above_limit limit acc =
    (* NOTE(@leostera:) we add 4 here since we want to consider the `: `
       between the header name and the value and the `\r\n` at the end *)
    acc
    |> List.exists (fun (k, v) -> 4 + String.length k + String.length v > limit)

  and do_parse_headers max_count max_length data acc =
    if List.length acc > max_count || header_above_limit max_length acc then
      raise_notrace Header_fields_too_large
    else
      (* we'll try to find the end of a header name *)
      match until ":" data with
      | exception Need_more_data -> (
          (* if we didn't find a header name.. *)
          match%bitstring data with
          (* ...and the next thing we see is the end of the headers, we're good *)
          | {| "\r\n\r\n" : 4 * 8 : string ; rest : -1 : bitstring |}
            when List.length acc = 0 ->
              trace (fun f -> f "end of headers! (no headers)");
              (acc, rest)
          (* ...or if the next thing we see a half end, and we had some headers, we're good*)
          | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring |}
            when List.length acc > 0 ->
              trace (fun f ->
                  f "end of headers! (header_count=%d)" (List.length acc));
              (acc, rest)
          (* ...anything else is probably a bad request *)
          | {| _ |} -> raise_notrace Bad_request)
      | h, data ->
          trace (fun f -> f "found header %s" (Bitstring.string_of_bitstring h));
          let header, rest = parse_header h data in
          let acc = header :: acc in
          do_parse_headers max_count max_length rest acc

  and parse_header h rest =
    let clean s = String.(s |> Bitstring.string_of_bitstring |> trim) in
    let v, rest = until "\r\n" rest in
    let h = clean h in
    let v = clean v in
    ((h, v), rest)
end

type state = {
  sniffed_data : Bytestring.t;
  is_keep_alive : bool;
  handler : Handler.t;
  are_we_tls : bool;
  config : Config.t;
  max_requests : int;
  requests_processed : int;
}

type error = [ `Excess_body_read | IO.io_error ]

exception Bad_port
exception Path_missing_leading_slash
exception Uri_too_long
exception Bad_request

let pp_err fmt t =
  match t with
  | `Excess_body_read -> Format.fprintf fmt "Excess body read"
  | #IO.io_error as io_err -> IO.pp_err fmt io_err

let make ~are_we_tls ~sniffed_data ~handler ~config () =
  {
    sniffed_data;
    handler;
    are_we_tls;
    config;
    is_keep_alive = false;
    max_requests = 0;
    requests_processed = 0;
  }

let handle_connection _conn state =
  trace (fun f -> f "switched to http1");
  Continue state

module StringSet = Set.Make (String)

let make_uri state (req : Trail.Request.t) =
  let h = Http.Header.get req.headers in

  let scheme, port =
    if state.are_we_tls then ("https", 443) else ("http", 80)
  in

  let uri =
    match (h "host", Uri.host req.uri) with
    | Some host, None -> Uri.of_string (scheme ^ "://" ^ host)
    | _ -> req.uri
  in
  let uri =
    match Uri.port uri with
    | Some _port -> uri
    | None -> Uri.with_port uri (Some port)
  in

  if Uri.port uri |> Option.value ~default:0 < 0 then raise_notrace Bad_port;

  let path = Uri.path req.uri in
  let query = Uri.query req.uri in

  (* If this is an OPTIONS request  it may come with a path to indicate that
     these are global options for requests. *)
  if req.meth = `OPTIONS && path = "*" then ()
    (* otherwise, we expect all requests to come with a leading slash. *)
  else if not (String.starts_with ~prefix:"/" path) then
    raise_notrace Path_missing_leading_slash;

  let uri = Uri.with_path uri path in
  let uri = Uri.with_query uri query in
  trace (fun f -> f "parse uri: %a" Uri.pp uri);
  uri

let send_close res ?(req = Trail.Request.make "noop") conn state =
  let _ = Adapter.send conn req res in
  Close state

let should_keep_alive (req : Trail.Request.t) =
  match (req.version, Http.Header.get req.headers "connection") with
  | _, Some "close" -> false
  | _, Some "keep-alive" -> true
  | `HTTP_1_1, _ -> true
  | _, _ -> false

let internal_server_error = send_close Trail.Response.(internal_server_error ())
let bad_request = send_close Trail.Response.(bad_request ())
let uri_too_long = send_close Trail.Response.(request_uri_too_long ())

let header_fields_too_large =
  send_close Trail.Response.(request_header_fields_too_large ())

let rec ensure_body_read conn req =
  match Adapter.read_body conn req with
  | Ok (_, _) -> Result.Ok ()
  | More (req, _) -> ensure_body_read conn req
  | Error (_, reason) -> Error reason

let maybe_keep_alive state conn (req : Trail.Request.t) =
  let requests_processed = state.requests_processed + 1 in
  let under_limit =
    state.max_requests == 0 || requests_processed < state.max_requests
  in
  trace (fun f ->
      f "requests_processed %d | under limit? %b | is_keep_alive? %b"
        requests_processed under_limit state.is_keep_alive);
  if under_limit && state.is_keep_alive then
    match ensure_body_read conn req with
    | Ok () -> Continue { state with sniffed_data = {%b||}; requests_processed }
    | Error `Closed -> Close state
    | Error reason -> Error (state, reason)
  else Close state

let handle_request state conn req =
  trace (fun f -> f "handle_request: %a" Trail.Request.pp req);
  match state.handler conn req with
  | Handler.Close _conn -> maybe_keep_alive state conn req
  | Handler.Upgrade (`websocket (upgrade_opts, handler)) ->
      let state = Ws.make ~upgrade_opts ~handler ~req ~conn () in
      let state = Ws.handshake req conn state in
      Switch (H { handler = (module Ws); state })
  | Handler.Upgrade `h2c ->
      trace (fun f -> f "upgrading to h2c");
      let state = Http2.make ~handler:state.handler ~conn () in
      let state = Http2.handshake req conn state in
      Switch (H { handler = (module Http2); state })

let run_handler state conn req =
  trace (fun f -> f "run_handler: %a" Trail.Request.pp req);
  match
    let uri = make_uri state req in
    let req = { req with uri } in
    let host =
      match (Http.Header.get req.headers "host", Uri.host uri) with
      | Some host, _ -> Some host
      | _, Some "" -> None
      | _, Some host -> Some host
      | _ -> None
    in
    let _content_length = Trail.Request.content_length req in
    let is_keep_alive = should_keep_alive req in
    trace (fun f -> f "connection is keep alive? %b" is_keep_alive);
    let state = { state with is_keep_alive } in
    (state, req.version, host)
  with
  | state, `HTTP_1_1, Some _host -> handle_request state conn req
  | state, `HTTP_1_0, _host -> handle_request state conn req
  | state, _http, _host -> bad_request ~req conn state

let handle_data data conn state =
  trace (fun f -> f "handling data: %a" Pid.pp (self ()));
  let data, state =
    let data = Bytestring.join state.sniffed_data data in
    (data, { state with sniffed_data = {%b||} })
  in
  trace (fun f -> f "handling data: %d bytes" (Bytestring.length data));

  match
    match Parser.parse ~config:state.config data with
    | `ok req -> run_handler state conn req
    | `bad_request -> bad_request conn state
    | `uri_too_long -> uri_too_long conn state
    | `header_fields_too_large -> header_fields_too_large conn state
    | `more data ->
        trace (fun f -> f "need more data: %d bytes" (Bytestring.length data));
        Continue { state with sniffed_data = data }
  with
  | exception
      (( Bad_request | Bad_port | Path_missing_leading_slash
       | Trail.Request.Invalid_content_header ) as exn) ->
      error (fun f ->
          f "bad_request: %s\n%s" (Printexc.to_string exn)
            (Printexc.get_backtrace ()));
      bad_request conn state
  | exception exn ->
      error (fun f ->
          f "internal_server_error: %s\n%s" (Printexc.to_string exn)
            (Printexc.get_backtrace ()));
      internal_server_error conn state
  | continue -> continue
