type Telemetry.event += Request_received of { req : Http.Request.t } [@@unboxed]

let request_received req = Telemetry.emit (Request_received { req })
