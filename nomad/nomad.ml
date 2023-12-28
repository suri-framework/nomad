module Http1 = Http1
module Request = Request

type handler = Handler.t

let start_link ?acceptor_count ~port ~handler () =
  Atacama.start_link ~port ?acceptor_count
    (module Connection_handler)
    (Connection_handler.make ~protocol:(module Protocol.Http1) ~handler ())
