module Adapter = Adapter
module Config = Config
module Handler = Handler
module Protocol = Protocol
module Request = Request

let start_link ?acceptor_count ?transport ?config ~port ~handler () =
  Atacama.start_link ~port ?acceptor_count ?transport
    (module Connection_handler)
    (Connection_handler.make ~handler ?config ())

let trail tr conn req =
  match Trail.handler (module Adapter) tr conn req with
  | `upgrade upgrade -> Handler.Upgrade upgrade
  | `close -> Handler.Close conn
