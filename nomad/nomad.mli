module Http1 = Http1

module Request : sig
  type t = {
    headers : Http.Header.t;
    meth : Http.Method.t;
    uri : Uri.t;
    version : Http.Version.t;
    encoding : Http.Transfer.encoding;
  }

  val pp : Format.formatter -> t -> unit
end

type handler = Atacama.Connection.t -> Request.t -> unit

val start_link :
  ?acceptor_count:int ->
  port:int ->
  handler:handler ->
  unit ->
  (Riot.Pid.t, [> `Supervisor_error ]) result
