open Riot

module type Intf = sig
  include Atacama.Handler.Intf

  val make : sniffed_data:string option -> handler:Handler.t -> unit -> state

  val handle_data :
    IO.Buffer.t ->
    Atacama.Connection.t ->
    state ->
    (state, 'b) Atacama.Handler.handler_result

  val to_string : Http.Response.t -> IO.Buffer.t
end

module Http1 : Intf = Http1
module Http2 : Intf = Http1
