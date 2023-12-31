open Riot
open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

module H2_stream = struct
  type Message.t += Frame of Frame.t

  let rec loop conn =
    match receive () with
    Frame frame -> 
      Logger.debug (fun f -> f "frame: %a" Frame.pp frame);
      let frame = Frame.serialize frame in
      let _ = Atacama.Connection.send conn frame in
      loop conn
      | _ -> loop conn

  let init stream_id conn =
    Logger.debug (fun f ->
        f "Stream %a stream_id=%d initialized" Pid.pp (self ()) stream_id);
    loop conn

  let start_link ~stream_id ~conn =
    let pid = spawn_link (fun () -> init stream_id conn) in
    Result.Ok pid

  let send_frame ~frame pid = send pid (Frame frame)
end

type settings = {
  header_table_size : int;
  initial_window_size : int;
  max_frame_size : int;
}

let default_settings =
  {
    header_table_size = 4_096;
    initial_window_size = 65_535;
    max_frame_size = 16_384;
  }

type state = {
  request : Http.Request.t;
  handler : Handler.t;
  buffer : IO.Buffer.t;
  conn : Atacama.Connection.t;
  settings : settings;
  streams : (Frame.stream_id, Pid.t) Hashtbl.t;
}

type error =
  [ `protocol_error of
    [ `continuation_frame_with_zero_stream_id
    | `data_frame_with_invalid_padding_length
    | `data_frame_with_zero_stream_id
    | `headers_frame_with_invalid_padding_length
    | `headers_frame_with_zero_stream_id
    | `invalid_payload_size_in_goaway_frame
    | `invalid_payload_size_in_ping_frame
    | `invalid_payload_size_in_priority_frame
    | `invalid_payload_size_in_rst_stream
    | `invalid_settings_frame_with_stream_id of Frame.stream_id
    | `invalid_stream_id_in_goaway_frame
    | `invalid_stream_id_in_ping_frame
    | `invalid_window_update_frame
    | `invalid_window_update_size_increment
    | `priority_frame_with_zero_stream_id
    | `push_promise_frame_received
    | `rst_stream_frame_with_zero_stream_id ]
  | `settings_error of
    [ `frame_size_too_large of int
    | `frame_size_too_small of int
    | `invalid_enable_push_value
    | `window_size_too_large of int ]
  | `could_not_initialize_connection ]

let err_to_str (err: error) =
  match err with
  | `could_not_initialize_connection -> "Could not initialize HTTP/2 connection"
  | `protocol_error (`invalid_settings_frame_with_stream_id sid) -> Format.sprintf "Protocol error: invalid SETTINGS frame with stream_id=%d" sid
  | `protocol_error `continuation_frame_with_zero_stream_id -> "Protocol error: invalid CONTINUATION frame with stream_id=0"
  | `protocol_error `headers_frame_with_zero_stream_id -> "Protocol error: invalid HEADERS frame with stream_id=0"
  | `protocol_error `data_frame_with_zero_stream_id ->"Protocol error: invalid DATA frame with stream_id=0"
  | `protocol_error `data_frame_with_invalid_padding_length -> "Protocol error: DATA frame with invalid padding length"
  | `settings_error (`frame_size_too_small size) -> Format.sprintf "SETTINGS error: max frame size of %d is too small " size
  | `settings_error (`frame_size_too_large size) -> Format.sprintf "SETTINGS error: max frame size of %d is too large " size
  | `settings_error (`window_size_too_large size) -> Format.sprintf "SETTINGS error: initial window size of %d is too large " size
  | `settings_error `invalid_enable_push_value -> "Protocol error: invalid `enable_push` frame settings value"
  | err -> Marshal.to_string err []

let pp_err fmt err = Format.fprintf fmt "%s" (err_to_str err)

let make ?(settings = default_settings) ~handler ~conn () =
  {
    request = Http.Request.make "";
    handler;
    buffer = IO.Buffer.with_capacity 4096;
    conn;
    settings;
    streams = Hashtbl.create 128;
  }

let[@warning "-8"] handshake conn state =
  let res =
    Trail.Response.(
      make `Switching_protocols
        ~headers:[ ("upgrade", "h2c"); ("connection", "Upgrade") ]
        ()
      |> to_buffer)
  in

  match Atacama.Connection.send conn res with
  | Ok _n -> state
  | _ -> failwith "could not handshake"

let handle_connection conn state =
  Logger.debug (fun f -> f "switched to http2");
  let frame = Frame.serialize Frame.empty_settings in
  match Atacama.Connection.send conn frame with
  | Ok _ -> Continue state
  | _ -> Error (state, `could_not_initialize_connection)

let handle_frame frame conn state =
  let stream_id = Frame.stream_id frame in
  let stream_pid =
    match Hashtbl.find_opt state.streams stream_id with
    | Some pid -> pid
    | None ->
        (* FIXME(@leostera): T_T *)
        let[@warning "-8"] Result.(Ok pid) =
          H2_stream.start_link ~stream_id ~conn
        in
        Hashtbl.add state.streams stream_id pid;
        pid
  in
  H2_stream.send_frame stream_pid ~frame;
  `continue (Continue state)

let handle_data data conn state =
  let data = IO.Buffer.to_string state.buffer ^ IO.Buffer.to_string data in
  data
  |> Stream.unfold
       (Frame.deserialize ~max_frame_size:state.settings.max_frame_size)
  |> Stream.reduce_while (Continue state) @@ fun frame state ->
     match (frame, state) with
     | `ok frame, Continue state -> handle_frame frame conn state
     | `more buffer, Continue state -> `halt (Continue { state with buffer })
     | `error reason, Continue state -> `halt (Error (state, reason))
     | _, _ -> failwith "Unexpected_frame_parsing_error"
