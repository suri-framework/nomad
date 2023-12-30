open Riot
open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

type state = {
  parser : Httpaf.Request.t Angstrom.Buffered.state;
  request : Trail.Request.t;
  sniffed_data : string option;
  handler : Handler.t;
}

type error = [ `noop ]

let pp_err _fmt _ = ()

let make ~sniffed_data ~handler () =
  {
    sniffed_data;
    parser = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.request;
    request = Trail.Request.make "";
    handler;
  }

let handle_connection _conn state =
  Logger.info (fun f -> f "switched to http1");
  Continue state

let req_from_httpaf req =
  let open Httpaf.Request in
  let version =
    Httpaf.Version.to_string req.version |> Http.Version.of_string
  in
  let headers = Httpaf.Headers.to_list req.headers in
  let meth = (req.meth :> Http.Method.t) in
  Trail.Request.make ~meth ~version ~headers req.target

let run_handler state conn req =
  let req = req_from_httpaf req in
  match state.handler conn req with
  | Handler.Close _conn when Trail.Request.is_keep_alive req -> Continue state
  | Handler.Close _conn -> Close state
  | Handler.Upgrade (`websocket (upgrade_opts, handler)) ->
      let state = Ws.make ~upgrade_opts ~handler ~req ~conn () in
      let state = Ws.handshake conn state in
      Switch (H { handler = (module Ws); state })
  | Handler.Upgrade `h2c ->
      let state = Http2.make ~sniffed_data:None ~handler:state.handler () in
      Switch (H { handler = (module Http2); state })

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
      match continue (`Bigstring data) with
      | Angstrom.Buffered.Done (_unconsumed, req) -> run_handler state conn req
      | Angstrom.Buffered.Fail (_, _, _) -> Close state
      | parser ->
          let state = { state with parser } in
          Continue state)
  | Angstrom.Buffered.Done (_unconsumed, req) -> run_handler state conn req
  | Angstrom.Buffered.Fail (_, _, _) -> Close state

let to_string (res : Http.Response.t) =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a %a\r\n%s%!" Http.Version.pp res.version Http.Status.pp
    res.status
    (Http.Header.to_string res.headers);
  IO.Buffer.of_string (Buffer.contents buf)
