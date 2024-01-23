open Riot
open Atacama.Handler
include Atacama.Handler.Default

open Riot.Logger.Make (struct
  let namespace = [ "nomad"; "ws" ]
end)

let ( let* ) = Result.bind

type state = {
  upgrade_opts : Trail.Sock.upgrade_opts;
  handler : Trail.Sock.t;
  req : Trail.Request.t;
  buffer : Bytestring.t;
  conn : Atacama.Connection.t;
}

type error = [ `Unknown_opcode of int ]

let pp_err _fmt _ = ()

let make ~upgrade_opts ~handler ~req ~conn () =
  { upgrade_opts; handler; req; buffer = Bytestring.empty; conn }

let handshake (req : Trail.Request.t) conn state =
  let[@warning "-8"] (Some client_key) =
    Http.Header.get req.headers "sec-websocket-key"
  in
  let concatenated_key = client_key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let hashed_key =
    Digestif.SHA1.(digest_string concatenated_key |> to_raw_string)
  in
  let server_key = Base64.encode_string hashed_key in

  let res =
    Trail.Response.(
      make `Switching_protocols
        ~headers:
          [
            ("upgrade", "websocket");
            ("connection", "Upgrade");
            ("sec-websocket-accept", server_key);
          ]
        ())
  in

  Adapter.send conn req res;
  state

let handle_connection conn state =
  debug (fun f -> f "switched to ws");
  info (fun f -> f "Request: %a" Trail.Request.pp state.req);
  match Trail.Sock.init state.handler conn with
  | `continue (conn, handler) -> Continue { state with conn; handler }
  | `error (conn, reason) -> Error ({ state with conn }, reason)

let rec send_frames state conn frames return =
  match frames with
  | [] -> return
  | frame :: frames -> (
      let data = Trail.Frame.serialize frame in
      match Atacama.Connection.send conn data with
      | Ok _n -> send_frames state conn frames return
      | Error `Eof ->
          error (fun f -> f "ws.error: end of file");
          `halt (Close state)
      | Error ((`Closed | `Timeout | `Process_down | `Unix_error _ | _) as err)
        ->
          error (fun f -> f "ws.error: %a" IO.pp_err err);
          `halt (Close state))

let handle_data data conn state =
  let data = Bytestring.to_string state.buffer ^ Bytestring.to_string data in
  Stream.unfold Trail.Frame.deserialize data
  |> Stream.reduce_while (Continue state) @@ fun frame state ->
     match (frame, state) with
     | `ok frame, Continue state -> (
         match[@warning "-8"]
           Trail.Sock.handle_frame state.handler frame conn
         with
         | `push (frames, handler) ->
             let state = { state with handler } in
             send_frames state conn frames (`continue (Continue state))
         | `continue (conn, handler) -> `continue (Continue { state with conn; handler})
         | `close conn -> `halt (Close { state with conn })
         | `error (conn, reason) -> `halt (Error ({ state with conn }, reason)))
     | `more buffer, Continue state -> `halt (Continue { state with buffer })
     | `error reason, Continue state -> `halt (Error (state, reason))
     | _, _ -> failwith "Unexpected_frame_parsing_error"

let handle_message msg conn state =
  match Trail.Sock.handle_message state.handler msg conn with
  | `continue (conn, handler) -> Continue { state with conn; handler }
  | `error (conn, reason) -> Error ({ state with conn }, reason)
  | `push (frames, handler) -> (
    let state = { state with handler } in
      match send_frames state conn frames (`continue (Continue state)) with
      | `continue cont -> cont
      | `halt res -> res)
