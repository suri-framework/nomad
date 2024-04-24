open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

type state = {
  enabled_protocols : [ `http1 | `http2 ] list;
  handler : Handler.t;
  config : Config.t;
}

type error = unit

let pp_err _fmt _ = ()

let make ?(enabled_protocols = [ `http1; `http2 ]) ?(config = Config.make ())
    ~handler () =
  { handler; config; enabled_protocols }

let handle_connection conn ({ enabled_protocols; handler; config; _ } as state)
    =
  match
    Negotiator.negotiated_protocol ~enabled_protocols ~config conn handler
  with
  | Ok new_handler -> Switch new_handler
  | _ -> Close state
