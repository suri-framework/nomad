open Atacama.Handler
include Atacama.Handler.Default

let ( let* ) = Result.bind

type state = {
  enabled_protocols : [ `http1 | `http2 ] list;
  handler : Handler.t;
}

type error = unit

let pp_err _fmt _ = ()

let make ?(enabled_protocols = [ `http1; `http2 ]) ~handler () =
  { handler; enabled_protocols }

let handle_connection conn ({ enabled_protocols; handler; _ } as state) =
  match Negotiator.negotiated_protocol ~enabled_protocols conn handler with
  | Ok new_handler -> Switch new_handler
  | _ -> Close state
