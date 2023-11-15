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
    let data = Bigstringaf.to_string data |> Bitstring.bitstring_of_string in
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
    Bigstringaf.of_string ~off:0 ~len:(Buffer.length buf) (Buffer.contents buf)
end

module Atacama_handler = struct
  open Atacama.Handler
  include Atacama.Handler.Default

  type state = {
    buffer : Bigstringaf.t;
    handler : Atacama.Socket.t -> Http.Request.t -> unit;
  }

  let join_bigstring a b =
    let a_len = Bigstringaf.length a in
    let b_len = Bigstringaf.length b in
    let buffer = Bigstringaf.create (a_len + b_len) in
    Bigstringaf.blit buffer ~src_off:0 a ~dst_off:0 ~len:a_len;
    Bigstringaf.blit buffer ~src_off:a_len b ~dst_off:0 ~len:b_len;
    buffer

  let handle_data data conn state =
    match Http1.parse data with
    | `more -> Continue { state with buffer = join_bigstring state.buffer data }
    | `ok req ->
        state.handler conn req;
        Close state
end
