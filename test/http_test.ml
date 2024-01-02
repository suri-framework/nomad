[@@@warning "-8"]

open Riot
open Trail

module Test : Application.Intf = struct
  let name = "test"

  let start () =
    Logger.set_log_level (Some Debug);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let hello_world (conn : Conn.t) =
      Logger.debug (fun f -> f "path: %s" (String.concat "." conn.req.path));
      match conn.req.path with
      | [ "echo_method" ] ->
          let body = conn.req.meth |> Http.Method.to_string in
          conn |> Conn.send_response `OK ~body
      | [ "echo_components" ] ->
          let scheme =
            Uri.scheme conn.req.uri |> Option.value ~default:"no-scheme"
          in
          let host = Uri.host_with_default ~default:"no-host" conn.req.uri in
          let port = Uri.port conn.req.uri |> Option.value ~default:0 in
          let path =
            conn.req.path |> List.map (Printf.sprintf "%S") |> String.concat ","
          in
          let query = Uri.query conn.req.uri |> Uri.encoded_of_query in
          let body =
            Format.sprintf
              {|{
                  "scheme": "%s",
                  "host": "%s",
                  "port": %d,
                  "path_info": [%s],
                  "query_string": "%s"
              }|}
              scheme host port path query
          in
          conn |> Conn.send_response `OK ~body
      | [ "send_big_body" ] ->
          let body = String.make 10_000 'a' in
          conn |> Conn.send_response `OK ~body
      | [ "send_content_encoding" ] ->
          conn
          |> Conn.with_header "content-encoding" "deflate"
          |> Conn.send_response `OK ~body:(String.make 10_000 'a')
      | [ "send_strong_etag" ] ->
          conn
          |> Conn.with_header "etag" "\"1234\""
          |> Conn.send_response `OK ~body:(String.make 10_000 'a')
      | [ "send_weak_etag" ] ->
          conn
          |> Conn.with_header "etag" "W/\"1234\""
          |> Conn.send_response `OK ~body:(String.make 10_000 'a')
      | [ "send_no_transform" ] ->
          conn
          |> Conn.with_header "cache-control" "no-transform"
          |> Conn.send_response `OK ~body:(String.make 10_000 'a')
      | [ "send_incorrect_content_length" ] ->
          conn
          |> Conn.with_header "content-length" "10001"
          |> Conn.send_response `OK ~body:(String.make 10_000 'a')
      | [ "send_200" ] -> conn |> Conn.send_response `OK
      | [ "send_204" ] ->
          conn |> Conn.send_response `No_content ~body:"bad content"
      | [ "send_301" ] -> conn |> Conn.send_response `Moved_permanently
      | [ "send_304" ] ->
          conn |> Conn.send_response `Not_modified ~body:"bad content"
      | [ "send_401" ] -> conn |> Conn.send_response `Forbidden
      | [ "send_chunked_200" ] ->
          conn |> Conn.send_chunked `OK |> Conn.chunk "OK" |> Conn.close
      | [ "erroring_chunk" ] ->
          let conn = conn |> Conn.send_chunked `OK |> Conn.chunk "OK" in
          Atacama.Connection.close conn.conn;
          conn |> Conn.chunk "NOT OK"
      | [ "send_file" ] ->
          let query = Uri.query conn.req.uri in
          Logger.debug (fun f -> f "%S" (Uri.encoded_of_query query));
          let off = List.assoc "offset" query |> List.hd |> int_of_string in
          let len = List.assoc "length" query |> List.hd |> int_of_string in
          conn
          |> Conn.send_file ~off ~len `OK "./test/bandit/test/support/sendfile"
      | [ "send_full_file" ] ->
          conn |> Conn.send_file `OK "./test/bandit/test/support/sendfile"
      | [ "send_full_file_204" ] ->
          conn
          |> Conn.send_file `No_content "./test/bandit/test/support/sendfile"
      | [ "send_inform" ] ->
          conn
          |> Conn.inform `Continue [ ("x-from", "inform") ]
          |> Conn.send_response `OK ~body:"Informer"
      | [ "report_version" ] ->
          let body = conn.req.version |> Http.Version.to_string in
          conn |> Conn.send_response `OK ~body
      | _ ->
          let body = conn.req.body |> Option.map IO.Buffer.to_string in
          conn |> Conn.send_response `Not_implemented ?body
    in

    let handler = Nomad.trail [ hello_world ] in

    Nomad.start_link ~port:2112 ~handler ()
end

let () = Riot.start ~apps:[ (module Logger); (module Test) ] ()
