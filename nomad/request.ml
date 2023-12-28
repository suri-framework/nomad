type t = {
  headers : Http.Header.t;
  meth : Http.Method.t;
  uri : Uri.t;
  version : Http.Version.t;
  encoding : Http.Transfer.encoding;
}

let pp fmt ({ headers; meth; uri; version; _ } : t) =
  let req = Http.Request.make ~meth ~headers ~version (Uri.to_string uri) in
  Http.Request.pp fmt req

let from_httpaf req =
  let open Httpaf.Request in
  let version =
    Httpaf.Version.to_string req.version |> Http.Version.of_string
  in
  let headers = Httpaf.Headers.to_list req.headers |> Http.Header.of_list in
  let meth = (req.meth :> Http.Method.t) in
  let encoding = Http.Header.get_transfer_encoding headers in
  let uri = Uri.of_string req.target in
  { headers; meth; uri; version; encoding }
