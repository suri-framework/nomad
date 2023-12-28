open Riot
open Atacama.Handler
include Atacama.Handler.Default

type state = {
  parser : Httpaf.Request.t Angstrom.Buffered.state;
  request : Http.Request.t;
  sniffed_data : string option;
  handler : Handler.t;
}

let make ~sniffed_data ~handler () =
  {
    sniffed_data;
    parser = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.request;
    request = Http.Request.make "";
    handler;
  }

let handle_data data conn state =
  let data, state =
    match state.sniffed_data with
    | Some sniff ->
        let data = IO.Buffer.to_string data in
        let data = sniff ^ data in
        (IO.Buffer.of_string data, { state with sniffed_data = None })
    | None -> (data, state)
  in

  match state.parser with
  | Angstrom.Buffered.Partial continue -> (
      let data = data |> IO.Buffer.as_cstruct |> Cstruct.to_bigarray in
      let parser = continue (`Bigstring data) in
      match parser with
      | Angstrom.Buffered.Done (_unconsumed, req) ->
          let req = Request.from_httpaf req in
          state.handler conn req;
          Close state
      | _ ->
          let state = { state with parser } in
          Continue state)
  | Angstrom.Buffered.Done (_unconsumed, req) ->
      let req = Request.from_httpaf req in
      state.handler conn req;
      Close state
  | Angstrom.Buffered.Fail (_, _, _) -> Close state

let to_string (res : Http.Response.t) =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a %a\r\n%s%!" Http.Version.pp res.version Http.Status.pp
    res.status
    (Http.Header.to_string res.headers);
  IO.Buffer.of_string (Buffer.contents buf)
