open Riot
open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

module Parser = struct
  exception Bad_request
  exception Need_more_data
  exception Uri_too_long
  exception Header_fields_too_large

  let until ?(max_len = -1) char data =
    let rec go char data len =
      let left = Bitstring.bitstring_length data in
      if left = 0 then raise_notrace Need_more_data
      else if max_len > 0 && len > max_len then raise_notrace Uri_too_long
      else
        match%bitstring data with
        | {| curr : 8 : string; rest : -1 : bitstring |} ->
            if String.equal curr char then (len, rest)
            else go char rest (len + 8)
    in
    let len, rest = go char data 0 in
    let captured = Bitstring.subbitstring data 0 len in
    (captured, rest)

  let rec parse ~(config : Config.t) data =
    let str = IO.Buffer.to_string data in
    let data = str |> Bitstring.bitstring_of_string in
    match do_parse ~config data with
    | exception Bad_request -> `bad_request
    | exception Uri_too_long -> `uri_too_long
    | exception Header_fields_too_large -> `header_fields_too_large
    | exception _ -> `more str
    | req, rest ->
        let body = Bitstring.string_of_bitstring rest in
        Logger.debug (fun f ->
            f "parsed_request: %a -> %S" Trail.Request.pp req body);
        `ok (req, IO.Buffer.of_string body)

  and do_parse ~config data =
    let meth, rest = parse_method config.max_line_request_length data in
    let path, rest = parse_path config.max_line_request_length rest in
    let version, rest = parse_protocol_version rest in
    let headers, rest =
      parse_headers config.max_header_count config.max_header_length rest
    in
    let req = Trail.Request.make ~meth ~version ~headers path in
    (req, rest)

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
        Logger.error (fun f -> f "found body");
        ([], rest)
    (* we found a beginning to the headers, but nothing more, so we need more data *)
    | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring|}
      when Bitstring.bitstring_length rest = 0 ->
        Logger.error (fun f -> f "need more data");
        raise_notrace Need_more_data
    (* we found a beginning to the headers, so we can try to parse them *)
    | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring |} ->
        Logger.error (fun f -> f "parsing headers");
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
              (acc, rest)
          (* ...or if the next thing we see a half end, and we had some headers, we're good*)
          | {| "\r\n" : 2 * 8 : string ; rest : -1 : bitstring |}
            when List.length acc > 0 ->
              (acc, rest)
          (* ...anything else is probably a bad request *)
          | {| _ |} -> raise_notrace Bad_request)
      | h, data ->
          Logger.debug (fun f ->
              f "found header %s" (Bitstring.string_of_bitstring h));
          let header, rest = parse_header h data in
          let acc = header :: acc in
          do_parse_headers max_count max_length rest acc

  and parse_header h rest =
    let clean s =
      String.(s |> Bitstring.string_of_bitstring |> lowercase_ascii |> trim)
    in
    let v, rest = until "\n" rest in
    let h = clean h in
    let v = clean v in
    ((h, v), rest)
end

type state = {
  sniffed_data : string option;
  handler : Handler.t;
  are_we_tls : bool;
  config : Config.t;
}

type error = [ `noop ]

exception Bad_port
exception Path_missing_leading_slash
exception Uri_too_long
exception Bad_request
exception Invalid_content_header

let pp_err _fmt _ = ()

let make ~are_we_tls ~sniffed_data ~handler ~config () =
  { sniffed_data; handler; are_we_tls; config }

let handle_connection _conn state =
  Logger.info (fun f -> f "switched to http1");
  Continue state

module StringSet = Set.Make (String)

(* TODO(@leostera): move this to Trail.Request.content_length *)
let content_length headers =
  match Http.Header.get headers "content-length" with
  | None -> None
  | Some value -> (
      let values =
        String.split_on_char ',' value
        |> List.map String.trim |> StringSet.of_list |> StringSet.to_list
        |> List.map Int64.of_string_opt
      in
      match values with
      | [ Some first ] when first > 0L -> Some (first |> Int64.to_int)
      | _ :: _ -> raise Invalid_content_header
      | _ -> None)

let rec read_body conn (req : Trail.Request.t) body =
  let max_body_length = content_length req.headers in
  let body_length = IO.Buffer.length body in
  match max_body_length with
  | Some limit when body_length = limit -> body
  | Some max_body_length -> (
      let limit = max_body_length - body_length in
      Logger.error (fun f -> f "reading body: %d" limit);
      match Atacama.Connection.receive ~limit conn with
      | Ok new_body ->
          let full_body =
            IO.Buffer.to_string body ^ IO.Buffer.to_string new_body
            |> IO.Buffer.of_string
          in
          if IO.Buffer.filled full_body = max_body_length then full_body
          else read_body conn req full_body
      | Error _ -> failwith "body read error")
  | None -> IO.Buffer.of_string ""

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
  Logger.error (fun f -> f "parse uri: %a" Uri.pp uri);
  uri

let send_close res conn state =
  let res = Trail.Response.(res |> to_buffer) in
  Logger.error (fun f -> f "response: %S" (IO.Buffer.to_string res));
  let _ = Atacama.Connection.send conn res in
  Close state

let bad_request conn state =
  Logger.error (fun f -> f "bad_request");
  send_close Trail.Response.(bad_request ()) conn state

let uri_too_long conn state =
  Logger.error (fun f -> f "uri_too_long");
  send_close Trail.Response.(request_uri_too_long ()) conn state

let header_fields_too_large conn state =
  Logger.error (fun f -> f "request_header_fields_too_large");
  send_close Trail.Response.(request_header_fields_too_large ()) conn state

let handle_request state conn req body =
  Logger.error (fun f -> f "handle_request: %a" Trail.Request.pp req);
  let req = Trail.Request.{ req with body = Some body } in
  let is_keep_alive =
    match Http.Header.get req.headers "connection" with
    | Some "keep-alive" -> true
    | _ -> false
  in
  match state.handler conn req with
  | Handler.Close _conn when is_keep_alive ->
      Logger.debug (fun f -> f "connection is keep alive, continuing");
      Continue { state with sniffed_data = None }
  | Handler.Close _conn -> Close state
  | Handler.Upgrade (`websocket (upgrade_opts, handler)) ->
      let state = Ws.make ~upgrade_opts ~handler ~req ~conn () in
      let state = Ws.handshake conn state in
      Switch (H { handler = (module Ws); state })
  | Handler.Upgrade `h2c ->
      Logger.debug (fun f -> f "upgrading to h2c");
      let state = Http2.make ~handler:state.handler ~conn () in
      let state = Http2.handshake conn state in
      Switch (H { handler = (module Http2); state })

let run_handler state conn req body =
  Logger.error (fun f -> f "run_handler: %a" Trail.Request.pp req);
  match
    let host = Http.Header.get req.headers "host" in
    let uri = make_uri state req in
    let body = read_body conn req body in
    (req.version, host, uri, body)
  with
  | exception
      (( Bad_request | Bad_port | Path_missing_leading_slash
       | Invalid_content_header ) as exn) ->
      Logger.error (fun f -> f "bad_request: %s" (Printexc.to_string exn));
      bad_request conn state
  | `HTTP_1_1, None, uri, body when Option.is_some (Uri.host uri) ->
      handle_request state conn { req with uri } body
  | `HTTP_1_1, Some _host, uri, body ->
      handle_request state conn { req with uri } body
  | `HTTP_1_0, _, uri, body -> handle_request state conn { req with uri } body
  | _ -> bad_request conn state

let handle_data data conn state =
  let data, state =
    match state.sniffed_data with
    | Some sniff ->
        let data = IO.Buffer.to_string data in
        let data = sniff ^ data in
        (IO.Buffer.of_string data, { state with sniffed_data = None })
    | None -> (data, state)
  in
  Logger.error (fun f -> f "handling data: %S" (IO.Buffer.to_string data));

  match Parser.parse ~config:state.config data with
  | `ok (req, body) -> run_handler state conn req body
  | `bad_request -> bad_request conn state
  | `uri_too_long -> uri_too_long conn state
  | `header_fields_too_large -> header_fields_too_large conn state
  | `more data -> Continue { state with sniffed_data = Some data }
