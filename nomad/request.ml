(*
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

  *)
