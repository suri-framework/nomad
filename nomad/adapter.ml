open Riot
open Trail

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

let gzip buf = gzip_string (IO.Buffer.to_string buf) |> IO.Buffer.of_string

let deflate buf =
  let str = deflate_string (IO.Buffer.to_string buf) in
  Logger.error (fun f -> f "deflate: %S" str);
  str |> IO.Buffer.of_string

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
  if IO.Buffer.length buf = 0 then (None, None)
  else (
    Logger.debug (fun f -> f "body: %s" (IO.Buffer.to_string buf));
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
      then (res.body, None)
      else
        match res.body with
        | Some body -> maybe_compress req body
        | None -> (None, None)
    in
    let headers =
      match encoding with
      | Some encoding -> Http.Header.add res.headers "content-encoding" encoding
      | None -> res.headers
    in
    let headers = Http.Header.add headers "vary" "accept-encoding" in

    let content_length =
      Option.map IO.Buffer.filled body |> Option.value ~default:0
    in
    let headers =
      if res.status = `No_content then
        Http.Header.remove headers "content-length"
      else if res.status != `Not_modified && content_length > 0 then
        Http.Header.replace headers "content-length"
          (Int.to_string content_length)
      else headers
    in
    let body =
      if
        req.meth = `HEAD || res.status = `No_content
        || res.status = `Not_modified
      then None
      else body
    in

    let buf = Response.to_buffer { res with headers; body } in
    Logger.debug (fun f -> f "res: %S" (IO.Buffer.to_string buf));
    let _ = Atacama.Connection.send conn buf in
    ()

let send_chunk conn (req : Request.t) buf =
  if req.meth != `HEAD then (
    let chunk =
      Format.sprintf "%x\r\n%s\r\n" (IO.Buffer.filled buf)
        (IO.Buffer.to_string buf)
    in
    Logger.debug (fun f -> f "sending chunk: %S" chunk);
    let chunk = IO.Buffer.of_string chunk in
    let _ = Atacama.Connection.send conn chunk in
    ())

let send_file conn (req : Request.t) (res : Response.t) ?off ?len ~path () =
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
  let res = { res with headers; body = None } in
  let _ = send conn req res in
  if res.status != `No_content then
    let _ = Atacama.Connection.send_file conn ?off ~len (File.open_read path) in
    ()

let close conn (req : Request.t) (res : Response.t) =
  if req.meth = `HEAD then ()
  else if res.status = `No_content then ()
  else
    let _ = Atacama.Connection.send conn (IO.Buffer.of_string "0\r\n\r\n") in
    ()
