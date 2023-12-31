open Riot

let is_flag_set n flags = flags land (1 lsl n) = 1
let is_flag_clear n flags = not (is_flag_set n flags)
let has_ack_bit = is_flag_set 0
let flags fs = List.fold_left (fun fs acc -> (fs lor acc) lsl 1) 0 fs
let min_frame_size = 16_384
let max_frame_size = 16_777_215
let max_window_size = 2_147_483_647

type stream_id = int

module Continuation = struct
  type t = { stream_id : stream_id; end_headers : bool; fragment : IO.Buffer.t }

  let has_headers_bit = is_flag_set 2
  let frame_type = 0x9

  let make ~flags ~stream_id ~payload =
    if stream_id = 0 then
      Error (`protocol_error `continuation_frame_with_zero_stream_id)
    else
      let fragment =
        IO.Buffer.of_string (Bitstring.string_of_bitstring payload)
      in
      Ok { stream_id; fragment; end_headers = has_headers_bit flags }

  let[@tail_mod_cons] rec parts t =
    let length = IO.Buffer.length t.fragment in
    if length <= max_frame_size then
      let data =
        IO.Buffer.to_string t.fragment |> Bitstring.bitstring_of_string
      in
      [ (frame_type, flags [ 2 ], t.stream_id, data) ]
    else
      let this_frame =
        let sub = IO.Buffer.sub ~len:max_frame_size t.fragment in
        IO.Buffer.to_string sub |> Bitstring.bitstring_of_string
      in
      let rest =
        let len = IO.Buffer.length t.fragment - max_frame_size in
        IO.Buffer.sub ~off:max_frame_size ~len t.fragment
      in
      let frame = (frame_type, 0x0, t.stream_id, this_frame) in
      frame :: parts { t with fragment = rest }
end

module Data = struct
  type t = {
    stream_id : stream_id;
    data : IO.Buffer.t;
    end_stream : bool;
    flags : int;
  }

  let frame_type = 0x0
  let has_end_stream_bit = is_flag_set 0
  let has_padding_bit = is_flag_set 3

  let padded_data ~flags ~stream_id ~payload =
    match%bitstring payload with
    | {| padding : 8 ; rest : -1 : string |} when String.length rest >= padding
      ->
        let data = String.sub rest 0 (String.length rest - padding) in
        let data = IO.Buffer.of_string data in
        Ok { stream_id; data; end_stream = has_end_stream_bit flags; flags }
    | {| _ |} -> Error (`protocol_error `data_frame_with_invalid_padding_length)

  let make ~flags ~stream_id ~payload =
    if stream_id = 0 then
      Error (`protocol_error `data_frame_with_zero_stream_id)
    else if has_padding_bit flags then padded_data ~flags ~stream_id ~payload
    else
      let data = IO.Buffer.of_string (Bitstring.string_of_bitstring payload) in
      Ok { stream_id; data; end_stream = has_end_stream_bit flags; flags }

  let[@tail_mod_cons] rec parts t =
    let length = IO.Buffer.length t.data in
    if length <= max_frame_size then
      let flags = flags (if t.end_stream then [ 0 ] else []) in
      let data = IO.Buffer.to_string t.data |> Bitstring.bitstring_of_string in
      [ (frame_type, flags, t.stream_id, data) ]
    else
      let this_frame =
        let sub = IO.Buffer.sub ~len:max_frame_size t.data in
        IO.Buffer.to_string sub |> Bitstring.bitstring_of_string
      in
      let rest =
        let len = IO.Buffer.length t.data - max_frame_size in
        IO.Buffer.sub ~off:max_frame_size ~len t.data
      in
      let frame = (frame_type, 0x0, t.stream_id, this_frame) in
      frame :: parts { t with data = rest }
end

module Settings = struct
  type settings = {
    header_table_size : int;
    initial_window_size : int;
    max_frame_size : int;
    max_header_list_size : int;
    max_concurrent_streams : int;
  }

  type t = { ack : bool; settings : settings }

  let default_settings =
    {
      header_table_size = 4_096;
      initial_window_size = 65_535;
      max_frame_size;
      max_header_list_size = Int.max_int;
      max_concurrent_streams = Int.max_int;
    }

  let empty = { ack = false; settings = default_settings }

  let parse_settings (payload : Bitstring.t) =
    let settings =
      payload
      |> Stream.unfold
           (function%bitstring
           | {| setting : 16 : int ; value : 32 : int ; rest : -1 : bitstring |}
             ->
               Some ((setting, Int32.to_int value), rest)
           | {| _ |} -> None)
      |> Stream.reduce_while (Ok default_settings) (fun (setting, value) acc ->
             match (setting, acc) with
             | 0x01, Ok settings ->
                 `continue (Ok { settings with header_table_size = value })
             | 0x02, Ok settings when value = 0 || value = 1 ->
                 `continue (Ok settings)
             | 0x02, _ ->
                 `halt (Error (`settings_error `invalid_enable_push_value))
             | 0x03, Ok settings ->
                 `continue (Ok { settings with max_concurrent_streams = value })
             | 0x04, Ok _settings when value > max_window_size ->
                 `halt (Error (`settings_error (`window_size_too_large value)))
             | 0x04, Ok settings ->
                 `continue (Ok { settings with initial_window_size = value })
             | 0x05, Ok _settings when value < min_frame_size ->
                 `halt (Error (`settings_error (`frame_size_too_small value)))
             | 0x05, Ok _settings when value > max_frame_size ->
                 `halt (Error (`settings_error (`frame_size_too_large value)))
             | 0x05, Ok settings ->
                 `continue (Ok { settings with max_frame_size = value })
             | 0x06, Ok settings ->
                 `continue (Ok { settings with max_header_list_size = value })
             | _, Ok settings -> `continue (Ok settings)
             | _, Error reason -> `halt (Error reason))
    in
    match settings with
    | Ok settings -> Ok { ack = true; settings }
    | Error reason -> Error reason

  let make ~flags ~stream_id ~payload =
    if stream_id != 0 then
      Error (`protocol_error (`invalid_settings_frame_with_stream_id stream_id))
    else if not (has_ack_bit flags) then parse_settings payload
    else Ok { ack = false; settings = default_settings }

  let parts t =
    let payload =
      [
        (t.settings.header_table_size, 4_096, 0x01);
        (t.settings.max_concurrent_streams, Int.max_int, 0x03);
        (t.settings.initial_window_size, 65_535, 0x04);
        (t.settings.max_frame_size, 16_384, 0x05);
        (t.settings.max_header_list_size, Int.max_int, 0x06);
      ]
      |> List.map (fun (value, default, code) ->
             if value = default then Bitstring.empty_bitstring
             else {%bitstring| code : 16; (Int32.of_int value) : 32 |})
      |> Bitstring.concat
    in
    [ (0x4, 0x0, 0, payload) ]
end

module Headers = struct
  type t = {
    stream_id : stream_id;
    end_stream : bool;
    end_headers : bool;
    exclusive_dependency : bool;
    stream_dependency : stream_id option;
    weight : int;
    fragment : IO.Buffer.t;
  }

  let pp fmt t =
    Format.fprintf fmt
      "stream_id=%d end_stream=%b end_headers=%b exclusive_dependency=%b \
       stream_dependency=%d weight=%d fragment=%S"
      t.stream_id t.end_stream t.end_headers t.exclusive_dependency
      (Option.value ~default:0 t.stream_dependency)
      t.weight
      (IO.Buffer.to_string t.fragment)

  let has_end_stream_bit = is_flag_set 0
  let has_end_headers_bit = is_flag_set 2
  let has_padding_bit = is_flag_set 3
  let has_priority_bit = is_flag_set 5
  let clear_priority_bit = is_flag_clear 5

  let do_make ~flags ~stream_id ~payload =
    let end_stream = has_end_stream_bit flags in
    let end_headers = has_end_headers_bit flags in
    match%bitstring payload with
    (* priority and padding *)
    | {| padding_length : 8;
         exclusive_dependency : 1; 
         stream_dependency : 31;
         weight : 8;
         rest : -1 : string |}
      when has_padding_bit flags && has_priority_bit flags
           && String.length rest >= padding_length ->
        Ok
          {
            stream_id;
            end_stream;
            end_headers;
            exclusive_dependency;
            stream_dependency = Some stream_dependency;
            weight;
            fragment =
              IO.Buffer.of_string
                (String.sub rest 0 (String.length rest - padding_length));
          }
    (* padding but no priority *)
    | {| padding_length : 8; rest : -1 : string |}
      when has_padding_bit flags && clear_priority_bit flags
           && String.length rest >= padding_length ->
        Ok
          {
            stream_id;
            end_stream;
            end_headers;
            exclusive_dependency = false;
            stream_dependency = None;
            weight = 0;
            fragment =
              IO.Buffer.of_string
                (String.sub rest 0 (String.length rest - padding_length));
          }
    (* other padding cases *)
    | {| _padding_length : 8 ; _ : -1 : string |} when has_padding_bit flags ->
        Error (`protocol_error `headers_frame_with_invalid_padding_length)
    (* priority but no padding *)
    | {| exclusive_dependency : 1; 
         stream_dependency : 31;
         weight : 8;
         fragment : -1 : string |}
      when has_priority_bit flags ->
        Ok
          {
            stream_id;
            end_stream;
            end_headers;
            exclusive_dependency;
            stream_dependency = Some stream_dependency;
            weight;
            fragment = IO.Buffer.of_string fragment;
          }
    (* no priority and no padding *)
    | {| fragment : -1 : string |} ->
        Ok
          {
            stream_id;
            end_stream;
            end_headers;
            exclusive_dependency = false;
            stream_dependency = None;
            weight = 0;
            fragment = IO.Buffer.of_string fragment;
          }

  let make ~flags ~stream_id ~payload =
    if stream_id = 0 then
      Error (`protocol_error `headers_frame_with_zero_stream_id)
    else do_make ~flags ~stream_id ~payload

  let parts t =
    let bits = if t.end_stream then [ 0 ] else [] in
    let fragment_length = IO.Buffer.length t.fragment in

    if fragment_length <= max_frame_size then
      let data =
        IO.Buffer.to_string t.fragment |> Bitstring.bitstring_of_string
      in
      [ (0x1, flags (2 :: bits), t.stream_id, data) ]
    else
      let this_frame =
        let sub = IO.Buffer.sub ~len:max_frame_size t.fragment in
        IO.Buffer.to_string sub |> Bitstring.bitstring_of_string
      in
      let rest =
        let len = IO.Buffer.length t.fragment - max_frame_size in
        IO.Buffer.sub ~off:max_frame_size ~len t.fragment
      in
      let frame = (0x1, 0x0, t.stream_id, this_frame) in
      frame
      :: Continuation.(
           parts
             { stream_id = t.stream_id; fragment = rest; end_headers = false })
end

module Priority = struct
  type t = {
    stream_id : stream_id;
    dependent_stream_id : stream_id;
    weight : int;
  }

  let make ~flags:_ ~stream_id ~payload =
    match%bitstring payload with
    | {| _rsv : 1 ; 
         dependent_stream_id : 31 ;
         weight : 8 |}
      when stream_id != 0 ->
        Ok { stream_id; dependent_stream_id; weight }
    | {| _ |} when stream_id = 0 ->
        Error (`protocol_error `priority_frame_with_zero_stream_id)
    | {| _ |} -> Error (`protocol_error `invalid_payload_size_in_priority_frame)

  let parts t =
    [
      ( 0x2,
        0x0,
        t.stream_id,
        {%bitstring| 0 : 1; t.dependent_stream_id : 31 ; t.weight : 8 |} );
    ]
end

module Rst_stream = struct
  (*
    no_error: 0x0,
    protocol_error: 0x1,
    internal_error: 0x2,
    flow_control_error: 0x3,
    settings_timeout: 0x4,
    stream_closed: 0x5,
    frame_size_error: 0x6,
    refused_stream: 0x7,
    cancel: 0x8,
    compression_error: 0x9,
    connect_error: 0xA,
    enhance_your_calm: 0xB,
    inadequate_security: 0xC,
    http_1_1_requires: 0xD
  *)
  type t = { stream_id : stream_id; error_code : int32 }

  let make ~flags:_ ~stream_id ~payload =
    match%bitstring payload with
    | {| error_code : 32 |} when stream_id != 0 -> Ok { stream_id; error_code }
    | {| _ |} when stream_id = 0 ->
        Error (`protocol_error `rst_stream_frame_with_zero_stream_id)
    | {| _ |} -> Error (`protocol_error `invalid_payload_size_in_rst_stream)

  let parts t = [ (0x3, 0x0, t.stream_id, {%bitstring| t.error_code : 32 |}) ]
end

module Push_promise = struct
  type t = unit

  let make ~flags:_ ~stream_id:_ ~payload:_ =
    Error (`protocol_error `push_promise_frame_received)
end

module Ping = struct
  type t = { ack : bool; payload : IO.Buffer.t }

  let has_ack_bit = is_flag_set 0

  let make ~flags ~stream_id ~payload =
    match%bitstring payload with
    | {| payload : 8 : string |} when stream_id = 0 ->
        Ok { ack = has_ack_bit flags; payload = IO.Buffer.of_string payload }
    | {| _ |} when stream_id != 0 ->
        Error (`protocol_error `invalid_stream_id_in_ping_frame)
    | {| _ |} -> Error (`protocol_error `invalid_payload_size_in_ping_frame)

  let parts t =
    let flags = if t.ack then flags [ 0 ] else 0 in
    let payload =
      IO.Buffer.to_string t.payload |> Bitstring.bitstring_of_string
    in
    [ (0x6, flags, 0, payload) ]
end

module Go_away = struct
  type t = {
    last_stream_id : stream_id;
    error_code : int32;
    debug_data : IO.Buffer.t;
  }

  let make ~flags:_ ~stream_id ~payload =
    match%bitstring payload with
    | {| _rsv : 1; last_stream_id : 31 ; error_code : 32; debug_data : -1 : string |}
      when stream_id = 0 ->
        Ok
          {
            last_stream_id;
            error_code;
            debug_data = IO.Buffer.of_string debug_data;
          }
    | {| _ |} when stream_id != 0 ->
        Error (`protocol_error `invalid_stream_id_in_goaway_frame)
    | {| _ |} -> Error (`protocol_error `invalid_payload_size_in_goaway_frame)

  let parts t =
    [
      ( 0x7,
        0x0,
        0,
        Bitstring.concat
          [
            {%bitstring| 0 : 1; t.last_stream_id : 31 ; t.error_code : 32 |};
            Bitstring.bitstring_of_string (IO.Buffer.to_string t.debug_data);
          ] );
    ]
end

module Window_update = struct
  type t = { stream_id : stream_id; size_increment : int }

  let make ~flags:_ ~stream_id ~payload =
    match%bitstring payload with
    | {| _rsv : 1 ; size_increment : 31 |} when size_increment = 0 ->
        Error (`protocol_error `invalid_window_update_size_increment)
    | {| _rsv : 1 ; size_increment : 31 |} -> Ok { stream_id; size_increment }
    | {| _ |} -> Error (`protocol_error `invalid_window_update_frame)

  let parts t =
    [ (0x8, 0, t.stream_id, {%bitstring| 0 : 1; t.size_increment : 31 |}) ]
end

type t =
  | Data of Data.t
  | Headers of Headers.t
  | Priority of Priority.t
  | Rst_stream of Rst_stream.t
  | Push_promise of Push_promise.t
  | Ping of Ping.t
  | Go_away of Go_away.t
  | Window_update of Window_update.t
  | Continuation of Continuation.t
  | Settings of Settings.t
  | Unknown of { stream_id : stream_id; flags : int; payload : IO.Buffer.t }

let pp fmt t =
  match t with
  | Data _ -> Format.fprintf fmt "Data"
  | Headers t -> Format.fprintf fmt "Headers(%a)" Headers.pp t
  | Priority _ -> Format.fprintf fmt "Priority"
  | Rst_stream _ -> Format.fprintf fmt "Rst_stream"
  | Push_promise _ -> Format.fprintf fmt "Push_promise"
  | Ping _ -> Format.fprintf fmt "Ping"
  | Go_away _ -> Format.fprintf fmt "Go_away"
  | Window_update _ -> Format.fprintf fmt "Window_update"
  | Continuation _ -> Format.fprintf fmt "Continuation"
  | Settings _ -> Format.fprintf fmt "Settings"
  | Unknown _ -> Format.fprintf fmt "Unknown"

let stream_id t =
  match t with
  | Data { stream_id; _ } -> stream_id
  | Headers { stream_id; _ } -> stream_id
  | Priority { stream_id; _ } -> stream_id
  | Rst_stream { stream_id; _ } -> stream_id
  | Settings _ -> 0x0
  | Push_promise _ -> 0x0
  | Ping _ -> 0x0
  | Go_away _ -> 0x0
  | Window_update { stream_id; _ } -> stream_id
  | Continuation { stream_id; _ } -> stream_id
  | Unknown { stream_id; _ } -> stream_id

let data t = Data t
let headers t = Headers t
let priority t = Priority t
let rst_stream t = Rst_stream t
let settings t = Settings t
let push_promise t = Push_promise t
let ping t = Ping t
let go_away t = Go_away t
let window_update t = Window_update t
let continuation t = Continuation t
let empty_settings = settings Settings.empty

let make ~type_ ~flags ~stream_id ~payload =
  Logger.debug (fun f -> f "Frame type=%d" type_);
  let result =
    match type_ with
    | 0x0 -> Data.make ~flags ~stream_id ~payload |> Result.map data
    | 0x1 -> Headers.make ~flags ~stream_id ~payload |> Result.map headers
    | 0x2 -> Priority.make ~flags ~stream_id ~payload |> Result.map priority
    | 0x3 -> Rst_stream.make ~flags ~stream_id ~payload |> Result.map rst_stream
    | 0x4 -> Settings.make ~flags ~stream_id ~payload |> Result.map settings
    | 0x5 ->
        Push_promise.make ~flags ~stream_id ~payload |> Result.map push_promise
    | 0x6 -> Ping.make ~flags ~stream_id ~payload |> Result.map ping
    | 0x7 -> Go_away.make ~flags ~stream_id ~payload |> Result.map go_away
    | 0x8 ->
        Window_update.make ~flags ~stream_id ~payload
        |> Result.map window_update
    | 0x9 ->
        Continuation.make ~flags ~stream_id ~payload |> Result.map continuation
    | _ ->
        let payload =
          IO.Buffer.of_string (Bitstring.string_of_bitstring payload)
        in
        Ok (Unknown { stream_id; flags; payload })
  in
  match result with Ok t -> `ok t | Error e -> `error e

let deserialize ~max_frame_size data =
  let data = Bitstring.bitstring_of_string data in
  match%bitstring data with
  | {| length : 24 ;
       type_  : 8  ;
       flags  : 8  ;
       _rsv   : 1  ;
       stream_id : 31 ;
       payload : (length * 8) : bitstring ;
       rest : -1 : string
      |}
    when length <= max_frame_size ->
      Some (make ~type_ ~flags ~stream_id ~payload, rest)
  | {| data : -1 : string  |} -> Some (`more (IO.Buffer.of_string data), "")

let parts frame =
  match frame with
  | Data t -> Data.parts t
  | Headers t -> Headers.parts t
  | Priority t -> Priority.parts t
  | Rst_stream t -> Rst_stream.parts t
  | Settings t -> Settings.parts t
  | Push_promise () -> []
  | Ping t -> Ping.parts t
  | Go_away t -> Go_away.parts t
  | Window_update t -> Window_update.parts t
  | Continuation t -> Continuation.parts t
  | Unknown { stream_id; flags; payload } ->
      let payload =
        IO.Buffer.to_string payload |> Bitstring.bitstring_of_string
      in
      [ (0x3, stream_id, flags, payload) ]

let serialize frame =
  let frames =
    parts frame
    |> List.map (fun (type_, stream_id, flags, payload) ->
           let length = Bitstring.bitstring_length payload in
           let%bitstring header =
             {| length    : 24;
                type_     :  8;
                flags     :  8;
                0         :  1;
                stream_id : 31 
            |}
           in
           Bitstring.concat [ header; payload ])
    |> Bitstring.concat
  in
  let frames = Bitstring.string_of_bitstring frames in
  Logger.debug (fun f -> f "htt2.frame.serialize %S" frames);
  IO.Buffer.of_string frames
