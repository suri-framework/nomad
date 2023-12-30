open Riot
include Atacama.Handler.Default

type state = {
  parser : Httpaf.Request.t Angstrom.Buffered.state;
  request : Http.Request.t;
  sniffed_data : string option;
  handler : Handler.t;
}

type error = [ `noop ]

let pp_err _fmt _ = ()

let make ~sniffed_data ~handler () =
  {
    sniffed_data;
    parser = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.request;
    request = Http.Request.make "";
    handler;
  }

let to_string (res : Http.Response.t) =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a %a\r\n%s%!" Http.Version.pp res.version Http.Status.pp
    res.status
    (Http.Header.to_string res.headers);
  IO.Buffer.of_string (Buffer.contents buf)
