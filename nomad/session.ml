open Atacama.Handler

let ( let* ) = Result.bind

type t =
  | Session : {
      protocol : (module Protocol.Intf with type state = 'state);
      state : 'state;
    }
      -> t

let make (type state) protocol sniffed_data handler =
  let (module Protocol : Protocol.Intf with type state = state) = protocol in
  Session { protocol; state = Protocol.make ~sniffed_data ~handler () }

let handle_data data conn (Session ({ protocol; state } as session)) =
  let (module Protocol) = protocol in
  match Protocol.handle_data data conn state with
  | Ok -> Ok
  | Continue state -> Continue (Session { session with state })
  | Continue_with_timeout (state, timeout) ->
      Continue_with_timeout (Session { session with state }, timeout)
  | Close state -> Close (Session { session with state })
  | Error (state, error) -> Error (Session { session with state }, error)
