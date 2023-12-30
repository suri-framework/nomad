module type Intf = sig
  include Atacama.Handler.Intf
end

module Http1 = Http1
module Http2 = Http2
module Ws = Ws
