open Riot

let to_string (res : Http.Response.t) =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a %a\r\n%s%!" Http.Version.pp res.version Http.Status.pp
    res.status
    (Http.Header.to_string res.headers);
  IO.Buffer.of_string (Buffer.contents buf)
