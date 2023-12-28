open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

type state = {
  session : Session.t;
  protocol : (module Protocol.Intf);
  enabled_protocols : [ `http1 | `http2 ] list;
  handler : Handler.t;
}

let make ~protocol ?(enabled_protocols = [ `http1; `http2 ]) ~handler () =
  {
    session = Session.make protocol None handler;
    handler;
    enabled_protocols;
    protocol = (module Protocol.Http1 : Protocol.Intf);
  }

let handle_connection conn ({ enabled_protocols; handler; _ } as state) =
  match Negotiator.negotiated_protocol ~enabled_protocols conn with
  | Ok session -> Continue { state with session = session handler }
  | _ -> Close state

let handle_data data conn ({ session; _ } as state) =
  match Session.handle_data data conn session with
  | Ok -> Ok
  | Continue session -> Continue { state with session }
  | Continue_with_timeout (session, timeout) ->
      Continue_with_timeout ({ state with session }, timeout)
  | Close session -> Close { state with session }
  | Error (session, error) -> Error ({ state with session }, error)
