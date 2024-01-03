open Trail

module Config : sig
  type t = {
    max_line_request_length : int;
    max_header_count : int;
    max_header_length : int;
    request_receive_timeout : int64;
  }

  val make :
    ?max_line_request_length:int ->
    ?max_header_count:int ->
    ?max_header_length:int ->
    ?request_receive_timeout:int64 ->
    unit ->
    t
end

module Handler : sig
  type response =
    | Close of Atacama.Connection.t
    | Upgrade : [ `websocket of Sock.upgrade_opts * Sock.t | `h2c ] -> response

  type t = Atacama.Connection.t -> Trail.Request.t -> response
end

val start_link :
  ?acceptor_count:int ->
  ?transport:Atacama.Transport.t ->
  ?config:Config.t ->
  port:int ->
  handler:Handler.t ->
  unit ->
  (Riot.Pid.t, [> `Supervisor_error ]) result

val trail :
  Trail.t -> Atacama.Connection.t -> Trail.Request.t -> Handler.response
