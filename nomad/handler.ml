type response =
  | Close of Atacama.Connection.t
  | Upgrade :
      [ `websocket of Trail.Sock.upgrade_opts * Trail.Sock.t | `h2c ]
      -> response

type t = Atacama.Connection.t -> Trail.Request.t -> response
