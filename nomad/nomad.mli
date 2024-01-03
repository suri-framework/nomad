open Trail

module Handler : sig
  type response =
    | Close of Atacama.Connection.t
    | Upgrade : [ `websocket of Sock.upgrade_opts * Sock.t | `h2c ] -> response

  type t = Atacama.Connection.t -> Trail.Request.t -> response
end

val start_link :
  ?acceptor_count:int ->
  ?transport:Atacama.Transport.t ->
  port:int ->
  handler:Handler.t ->
  unit ->
  (Riot.Pid.t, [> `Supervisor_error ]) result

val trail :
  Trail.t -> Atacama.Connection.t -> Trail.Request.t -> Handler.response
