[@@@warning "-8"]

open Riot

module Http = struct
  let until char data = 
    let rec go char data len =
      match%bitstring data with
      | {| curr : 8 : string; rest : -1 : bitstring |} ->
          if String.equal curr char 
          then len, rest
          else go char rest (len+8)
    in
    let len, rest = go char data 0 in
    let captured = Bitstring.subbitstring data 0 len in
    captured, rest

  let rec parse data =
    let meth, rest = parse_method data in
    let path, rest = parse_path rest in
    let version, rest = parse_protocol_version rest in
    let headers, _body = parse_headers rest [] in 
    let req = Http.Request.make ~meth ~version ~headers path in
    Logger.debug (fun f -> f "req: %a" Http.Request.pp req);
    req

  and parse_method data = 
    let meth, rest = until " " data in
    let meth = Bitstring.string_of_bitstring meth in
    Http.Method.of_string meth, rest

  and parse_path data =
    let path, rest = until " " data in
    Bitstring.string_of_bitstring path, rest

  and parse_protocol_version: Bitstring.t -> Http.Version.t * Bitstring.t =
    function%bitstring
    | {| "HTTP/1.0" : 8*8 : string; rest : -1 : bitstring |} -> `HTTP_1_0, rest
    | {| "HTTP/1.1" : 8*8 :string; rest : -1 : bitstring |} -> `HTTP_1_1, rest

  and parse_headers data acc = 
    let header, rest = parse_header data in
    let acc = header :: acc in
    match%bitstring rest with
    | {| "\r\n" : 2*8 : string ; rest : -1 : bitstring |} -> Http.Header.of_list acc, rest
    | {| "\n" : 8 : string ; rest : -1 : bitstring |} -> Http.Header.of_list acc, rest
    | {| _ |} -> parse_headers rest acc

  and parse_header data =
    let h, rest = until ":" data in
    let v, rest = until "\n" rest in
    let h = Bitstring.string_of_bitstring h |> String.lowercase_ascii |> String.trim in
    let v = Bitstring.string_of_bitstring v |> String.lowercase_ascii |> String.trim in
    (h, v), rest
end

module Caravan_nomad_handler = struct
  open Caravan.Handler
  include Caravan.Handler.Default

  let handle_data data _socket state =
    let _req = Http.parse (Bigstringaf.to_string data |> Bitstring.bitstring_of_string) in
    Continue state
end

let main () =
  Riot.Net.Socket.Logger.set_log_level (Some Trace);
  Logger.set_log_level (Some Trace);
  let (Ok _) = Logger.start ~print_source:true () in
  sleep 0.1;
  Logger.info (fun f -> f "starting nomad caravan");
  let (Ok pid) = Caravan.start_link ~port:2112 (module Caravan_nomad_handler) in
  wait_pids [ pid ]

let () = Riot.run @@ main

