open Atacama
open Trail

module Handler : sig
  type response =
    | Close of Atacama.Connection.t
    | Upgrade : [ `websocket of Sock.upgrade_opts * Sock.t | `h2c ] -> response

  type t = Atacama.Connection.t -> Trail.Request.t -> response
end

val start_link :
  ?acceptor_count:int ->
  port:int ->
  handler:Handler.t ->
  unit ->
  (Riot.Pid.t, [> `Supervisor_error ]) result

module Protocol : sig
  module type Intf = sig
    include Atacama.Handler.Intf
  end

  module Http1 : sig
    include Intf

    val make : sniffed_data:string option -> handler:Handler.t -> unit -> state
    val to_string : Http.Response.t -> Riot.IO.Buffer.t
  end

  module Http2 : Intf

  module Ws : sig
    include Intf

    val make :
      upgrade_opts:Sock.upgrade_opts ->
      handler:Sock.t ->
      req:Trail.Request.t ->
      conn:Connection.t ->
      unit ->
      state
  end
end

val trail :
  Trail.t -> Atacama.Connection.t -> Trail.Request.t -> Handler.response
