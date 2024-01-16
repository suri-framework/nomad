open Riot
open Trail

module Logger = Logger.Make (struct
  let namespace = [ "nomad"; "http1"; "adapter" ]
end)

open Logger

let ( let* ) = Result.bind

let rec split ?(left = {%b||}) str =
  match%b str with
  | {| "\r\n"::bytes, rest::bytes |} -> [ left; rest ]
  | {| c::utf8, rest::bytes |} -> split ~left:(Bytestring.join c left) rest

let deflate_string str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Zl.Higher.compress ~level:9 ~w ~q ~refill ~flush i o;
  Buffer.contents r

let gzip_string str =
  let time () = 2112l in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:16 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let cfg = Gz.Higher.configuration Gz.Unix time in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Gz.Higher.compress ~w ~q ~level:9 ~refill ~flush () cfg i o;
  Buffer.contents r

let gzip buf = gzip_string (Bytestring.to_string buf) |> Bytestring.of_string

let deflate buf =
  let str = deflate_string (Bytestring.to_string buf) in
  str |> Bytestring.of_string

let has_custom_content_encoding (res : Response.t) =
  Http.Header.get res.headers "content-encoding" |> Option.is_some

let has_weak_etag (res : Response.t) =
  match Http.Header.get res.headers "etag" with
  | Some etag -> String.starts_with ~prefix:"W/" etag
  | None -> false

let has_strong_etag (res : Response.t) =
  match Http.Header.get res.headers "etag" with
  | Some etag -> not (String.starts_with ~prefix:"W/" etag)
  | None -> false

let has_no_transform (res : Response.t) =
  match Http.Header.get res.headers "cache-control" with
  | Some "no-transform" -> true
  | _ -> false

let maybe_compress (req : Request.t) buf =
  if Bytestring.length buf = 0 then (None, None)
  else (
    debug (fun f -> f "body: %s" (Bytestring.to_string buf));
    let accepted_encodings =
      Http.Header.get req.headers "accept-encoding"
      |> Option.map (fun enc -> String.split_on_char ',' enc)
      |> Option.value ~default:[] |> List.map String.trim
    in
    let accepts_deflate = List.mem "deflate" accepted_encodings in
    let accepts_gzip = List.mem "gzip" accepted_encodings in
    let accepts_x_gzip = List.mem "x-gzip" accepted_encodings in
    if accepts_deflate then (Some (deflate buf), Some "deflate")
    else if accepts_gzip then (Some (gzip buf), Some "gzip")
    else if accepts_x_gzip then (Some (gzip buf), Some "x-gzip")
    else (Some buf, None))

let send conn (req : Request.t) (res : Response.t) =
  if req.version = `HTTP_1_0 && res.status = `Continue then ()
  else
    let body, encoding =
      if
        has_custom_content_encoding res
        || has_strong_etag res || has_no_transform res
      then (Some res.body, None)
      else maybe_compress req res.body
    in
    let headers =
      match encoding with
      | Some encoding -> Http.Header.add res.headers "content-encoding" encoding
      | None -> res.headers
    in
    let headers = Http.Header.add headers "vary" "accept-encoding" in
    let is_chunked =
      Http.Header.get_transfer_encoding headers = Http.Transfer.Chunked
    in

    let body_len =
      Option.map Bytestring.length body
      |> Option.value ~default:0 |> Int.to_string
    in
    let headers =
      let content_length = Http.Header.get headers "content-length" in
      match (content_length, res.status) with
      | _, (`No_content | `Not_modified) ->
          Http.Header.remove headers "content-length"
      | None, _ when not is_chunked ->
          Http.Header.replace headers "content-length" body_len
      | _ -> headers
    in

    let headers =
      match Http.Header.get headers "date" with
      | Some _ -> headers
      | None ->
          let now = Ptime_clock.now () in
          let (y, mon, d), ((h, m, s), _ns) = Ptime.to_date_time now in
          let day =
            match Ptime.weekday ?tz_offset_s:None now with
            | `Sat -> "Sat"
            | `Fri -> "Fri"
            | `Mon -> "Mon"
            | `Wed -> "Wed"
            | `Tue -> "Tue"
            | `Sun -> "Sun"
            | `Thu -> "Thu"
          in
          let[@warning "-8"] mon =
            match mon with
            | 0 -> "Jan"
            | 1 -> "Feb"
            | 2 -> "Mar"
            | 3 -> "Apr"
            | 4 -> "May"
            | 5 -> "Jun"
            | 6 -> "Jul"
            | 7 -> "Aug"
            | 8 -> "Sep"
            | 9 -> "Oct"
            | 10 -> "Nov"
            | 11 -> "Dec"
          in
          let now =
            Format.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day d mon y h m s
          in
          Logger.debug (fun f -> f "Adding date header: %S" now);
          Http.Header.add headers "date" now
    in

    let body =
      if
        req.meth = `HEAD || res.status = `No_content
        || res.status = `Not_modified
      then None
      else body
    in

    let buf =
      let version =
        res.version |> Http.Version.to_string |> Httpaf.Version.of_string
      in
      let status = res.status |> Http.Status.to_int |> Httpaf.Status.of_code in
      let headers = headers |> Http.Header.to_list |> Httpaf.Headers.of_list in
      let res = Httpaf.Response.create ~version ~headers status in
      let buf = Faraday.create (1024 * 4) in
      Httpaf.Httpaf_private.Serialize.write_response buf res;

      (match body with
      | Some body -> Faraday.write_string buf (Bytestring.to_string body)
      | _ -> ());

      let s = Faraday.serialize_to_string buf in
      Bytestring.of_string s
    in

    debug (fun f -> f "res: %S" (Bytestring.to_string buf));
    Atacama.Connection.send conn buf
    |> Result.get_ok

let send_chunk conn (req : Request.t) buf =
  if req.meth = `HEAD then ()
  else
    let chunk =
      Format.sprintf "%x\r\n%s\r\n" (Bytestring.length buf)
        (Bytestring.to_string buf)
    in
    debug (fun f -> f "sending chunk: %S" chunk);
    let chunk = Bytestring.of_string chunk in
    let _ = Atacama.Connection.send conn chunk in
    ()

let close_chunk conn =
  let chunk = Bytestring.of_string "0\r\n\r\n" in
  let _ = Atacama.Connection.send conn chunk in
  ()

let send_file _conn (_req : Request.t) (_res : Response.t) ?off:_ ?len:_ ~path:_
    () =
  (*
  let len =
    match len with
    | Some len -> len
    | None ->
        let stat = File.stat path in
        stat.st_size
  in
  let headers =
    Http.Header.replace res.headers "content-length" (Int.to_string len)
  in
  let res = { res with headers; body = Bytestring.empty } in
  let _ = send conn req res in
  if
    req.meth != `HEAD && res.status != `No_content
    && res.status != `Not_modified
  then
    let _ = Atacama.Connection.send_file conn ?off ~len (File.open_read path) in
    ()
    *)
  ()

let close conn (req : Request.t) (res : Response.t) =
  if req.meth = `HEAD then ()
  else if res.status = `No_content then ()
  else
    let _ = Atacama.Connection.send conn (Bytestring.of_string "0\r\n\r\n") in
    ()

open Trail

let rec read_body ?limit ?(read_size = 1_024 * 1_024) conn (req : Request.t) =
  match Request.body_encoding req with
  | Http.Transfer.Chunked -> (
      debug (fun f -> f "reading chunked body");
      match
        read_chunked_body ~read_size ~buffer:req.buffer ~body:Bytestring.empty
          conn req
      with
      | Ok (body, buffer) ->
          debug (fun f ->
              f "read chunked_body: buffer=%d" (Bytestring.length buffer));
          Adapter.Ok ({ req with buffer }, body)
      | Error reason -> Adapter.Error (req, reason))
  | _ -> (
      debug (fun f -> f "reading content-length body");
      match read_content_length_body ?limit ~read_size conn req with
      | Ok (body, buffer, body_remaining) ->
          debug (fun f ->
              f "read chunked_body: body_remaning=%d buffer=%d" body_remaining
                (Bytestring.length buffer));
          let req = { req with buffer; body_remaining } in
          if body_remaining = 0 && Bytestring.length buffer = 0 then (
            debug (fun f -> f "read chunked_body: ok");
            let req = { req with buffer; body_remaining = -1 } in
            Adapter.Ok (req, body))
          else (
            debug (fun f -> f "read chunked_body: more");
            Adapter.More (req, body))
      | Error reason -> Adapter.Error (req, reason))

and read_chunked_body ~read_size ~buffer ~body conn req =
  let parts = split buffer in
  debug (fun f -> f "body_size: %d" (Bytestring.length body));
  debug (fun f -> f "buffer: %d" (Bytestring.length buffer));
  debug (fun f ->
      f "total_read: %d" (Bytestring.length buffer + Bytestring.length body));
  debug (fun f ->
      match parts with
      | size :: _ -> f "chunk_size: 0x%s" (Bytestring.to_string size)
      | _ -> ());

  match parts with
  | [ zero; _ ] when String.equal (Bytestring.to_string zero) "0" ->
      debug (fun f -> f "read_chunked_body: last chunk!");
      Ok (body, buffer)
  | [ chunk_size; chunk_data ] -> (
      let chunk_size =
        Int64.(of_string ("0x" ^ Bytestring.to_string chunk_size) |> to_int)
      in
      debug (fun f -> f "read_chunked_body: chunk_size=%d" chunk_size);
      let binstr_data = Bytestring.to_string chunk_data in
      debug (fun f ->
          f "read_chunked_body: (%d bytes)" (String.length binstr_data));
      let binstr_data = binstr_data |> Bitstring.bitstring_of_string in
      match%bitstring binstr_data with
      | {| next_chunk : (chunk_size * 8) : string ;
           "\r\n" : 2 * 8 : string ;
           rest : -1 : bitstring |}
        ->
          debug (fun f -> f "read_chunked_body: read full chunk");
          debug (fun f ->
              f "read_chunked_body: rest=%d" (Bitstring.bitstring_length rest));
          let rest =
            Bytestring.of_string (Bitstring.string_of_bitstring rest)
          in
          let next_chunk = Bytestring.of_string next_chunk in
          let body = Bytestring.join body next_chunk in
          read_chunked_body ~read_size ~buffer:rest ~body conn req
      | {| _ |} ->
          let left_to_read = chunk_size - Bytestring.length chunk_data in
          debug (fun f ->
              f "read_chunked_body: reading more data left_to_read=%d"
                left_to_read);
          let* chunk =
            if left_to_read > 0 then read ~to_read:left_to_read ~read_size conn
            else Atacama.Connection.receive conn
          in
          let buffer = Bytestring.join buffer chunk in
          read_chunked_body ~read_size ~buffer ~body conn req)
  | _ ->
      debug (fun f -> f "read_chunked_body: need more data");
      let* chunk = Atacama.Connection.receive conn in
      let buffer = Bytestring.join buffer chunk in
      read_chunked_body ~read_size ~buffer ~body conn req

and read_content_length_body ?limit ~read_size conn req =
  let buffer = req.buffer in
  let limit = Option.value ~default:req.body_remaining limit in
  let to_read = limit - Bytestring.length buffer in
  debug (fun f ->
      f "read_content_length_body: up to limit=%d with preread_buffer=%d" limit
        (Bytestring.length buffer));
  match req.body_remaining with
  | n when n < 0 || to_read < 0 ->
      debug (fun f -> f "read_content_length_body: excess body");
      Error `Excess_body_read
  | 0 when Bytestring.length buffer >= limit ->
      debug (fun f -> f "read_content_length_body: can answer with buffer");
      let len = Int.min limit (Bytestring.length buffer) in
      let body = Bytestring.sub ~off:0 ~len buffer in
      Ok (body, Bytestring.empty, 0)
  | remaining_bytes ->
      let to_read =
        Int.min (limit - Bytestring.length buffer) remaining_bytes
      in
      debug (fun f -> f "read_content_length_body: need to read %d" to_read);
      let* chunk = read ~to_read ~read_size conn in
      let body = Bytestring.join buffer chunk in
      let body_remaining = remaining_bytes - Bytestring.length body in
      Ok (body, Bytestring.empty, body_remaining)

and read ~read_size ~to_read ?(buffer = Bytestring.empty) conn =
  if to_read = 0 then Ok Bytestring.empty
  else
    let* chunk = Atacama.Connection.receive ~limit:to_read ~read_size conn in
    let remaining_bytes = to_read - Bytestring.length chunk in
    let buffer = Bytestring.join buffer chunk in
    debug (fun f -> f "read: remaining_bytes %d" remaining_bytes);
    debug (fun f -> f "read: buffer=%d" (Bytestring.length buffer));
    if remaining_bytes > 0 then
      read ~read_size ~to_read:remaining_bytes ~buffer conn
    else Ok buffer
