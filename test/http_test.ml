[@@@warning "-8"]

open Riot
open Trail

module Logger = Logger.Make (struct
  let namespace = [ "http_test" ]
end)

module Test : Application.Intf = struct
  let start () =
    Logger.set_log_level (Some Info);
    sleep 0.1;
    Logger.info (fun f -> f "starting nomad server");

    let hello_world (conn : Conn.t) =
      Logger.debug (fun f ->
          f "http_test.path: %S" (String.concat "." conn.req.path));
      match conn.req.path with
      | [] -> conn |> Conn.send_response `OK {%b|"hello world"|}
      | [ "echo_method" ] ->
          let body =
            conn.req.meth |> Http.Method.to_string |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
      | [ "*" ] when conn.req.meth = `OPTIONS ->
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
            |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
      | [ "peer_data" ] ->
          let Conn.{ ip; port } = conn.peer in
          let body =
            Format.sprintf {|{ "address": "%s", "port": %d }|}
              (Net.Addr.to_string ip) port
            |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
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
            |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
      | [ "send_big_body" ] ->
          let body = String.make 10_000 'a' |> Bytestring.of_string in
          conn |> Conn.send_response `OK body
      | [ "send_content_encoding" ] ->
          conn
          |> Conn.with_header "content-encoding" "deflate"
          |> Conn.send_response `OK
               (String.make 10_000 'a' |> Bytestring.of_string)
      | [ "send_strong_etag" ] ->
          conn
          |> Conn.with_header "etag" "\"1234\""
          |> Conn.send_response `OK
               (String.make 10_000 'a' |> Bytestring.of_string)
      | [ "send_weak_etag" ] ->
          conn
          |> Conn.with_header "etag" "W/\"1234\""
          |> Conn.send_response `OK
               (String.make 10_000 'a' |> Bytestring.of_string)
      | [ "send_no_transform" ] ->
          conn
          |> Conn.with_header "cache-control" "no-transform"
          |> Conn.send_response `OK
               (String.make 10_000 'a' |> Bytestring.of_string)
      | [ "send_incorrect_content_length" ] ->
          conn
          |> Conn.with_header "content-length" "10001"
          |> Conn.send_response `OK
               (String.make 10_000 'a' |> Bytestring.of_string)
      | [ "send_200" ] -> conn |> Conn.send_status `OK
      | [ "send_204" ] ->
          conn |> Conn.send_response `No_content {%b|"bad content"|}
      | [ "send_301" ] -> conn |> Conn.send_status `Moved_permanently
      | [ "send_304" ] ->
          conn |> Conn.send_response `Not_modified {%b|"bad content"|}
      | [ "send_401" ] -> conn |> Conn.send_status `Unauthorized
      | [ "send_stream" ] ->
          let chunks = Seq.repeat {%b|"hello world"|} in

          chunks
          |> Seq.fold_left
               (fun conn chunk -> Conn.chunk chunk conn)
               (Conn.send_chunked `OK conn)
          |> Conn.close
      | [ "send_chunked_200" ] ->
          conn |> Conn.send_chunked `OK
          |> Conn.chunk {%b|"OK"|}
          |> Conn.close
      | [ "erroring_chunk" ] ->
          let conn =
            conn |> Conn.send_chunked `OK |> Conn.chunk {%b|"OK"|}
          in
          Atacama.Connection.close conn.conn;
          conn |> Conn.chunk {%b|"NOT OK"|}
      | [ "send_file" ] ->
          let query = Uri.query conn.req.uri in
          Logger.debug (fun f -> f "%S" (Uri.encoded_of_query query));
          let off = List.assoc "offset" query |> List.hd |> int_of_string in
          let len = List.assoc "length" query |> List.hd |> int_of_string in
          conn
          |> Conn.send_file ~off ~len `OK
               ~path:"./test/bandit/test/support/sendfile"
      | [ "send_full_file" ] ->
          conn
          |> Conn.send_file `OK
               ~path:"./test/bandit/test/support/sendfile"
      | [ "send_full_file_204" ] ->
          conn
          |> Conn.send_file `No_content
               ~path:"./test/bandit/test/support/sendfile"
      | [ "send_full_file_304" ] ->
          conn
          |> Conn.send_file `Not_modified
               ~path:"./test/bandit/test/support/sendfile"
      | [ "send_inform" ] ->
          conn
          |> Conn.inform `Continue [ ("x-from", "inform") ]
          |> Conn.send_response `OK {%b|"Informer"|}
      | [ "report_version" ] ->
          let body =
            conn.req.version |> Http.Version.to_string |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
      | "expect_headers" :: _ ->
          conn |> Conn.send_response `OK {%b|"OK"|}
      | "expect_no_body" :: [] ->
          let[@warning "-8"] (Conn.Ok (conn, body)) = Conn.read_body conn in
          assert (Bytestring.to_string body = "");
          conn |> Conn.send_response `OK {%b|"OK"|}
      | "expect_body" :: [] ->
          let expected_content_length = "8000000" in
          let content_length =
            Http.Header.get conn.req.headers "content-length" |> Option.get
          in
          Logger.debug (fun f -> f "content_length: %s" content_length);
          let expected_body =
            List.init 800000 (fun _ -> "0123456789") |> String.concat ""
          in
          let[@warning "-8"] (Conn.Ok (conn, actual_body)) =
            Conn.read_body conn
          in
          let actual_body = Bytestring.to_string actual_body in
          Logger.debug (fun f -> f "actual_ %d" (String.length actual_body));
          assert (String.equal content_length expected_content_length);
          assert (String.equal actual_body expected_body);
          conn |> Conn.send_response `OK {%b|"OK"|}
      | "expect_body_with_multiple_content_length" :: [] ->
          let expected_content_length = "8000000,8000000,8000000" in
          let content_length =
            Http.Header.get conn.req.headers "content-length" |> Option.get
          in
          Logger.debug (fun f -> f "content_length: %s" content_length);
          let expected_body =
            List.init 8_000_000 (fun _ -> "a") |> String.concat ""
          in
          let[@warning "-8"] (Conn.Ok (conn, actual_body)) =
            Conn.read_body conn
          in
          let actual_body = Bytestring.to_string actual_body in
          Logger.debug (fun f -> f "actual_ %d" (String.length actual_body));
          assert (String.equal content_length expected_content_length);
          assert (String.equal actual_body expected_body);
          conn |> Conn.send_response `OK {%b|"OK"|}
      | "read_one_byte_at_a_time" :: [] ->
          let[@warning "-8"] (Conn.Ok (conn, body)) =
            Conn.read_body ~limit:5 conn
          in
          conn |> Conn.send_response `OK body
      | "error_catcher" :: [] ->
          let[@warning "-8"] (Conn.Error (conn, reason)) =
            Conn.read_body conn
          in
          let body =
            (match reason with
            | `Excess_body_read -> "Excess_body_read"
            | (`Closed | `Process_down | `Timeout | `Unix_error _) as reason ->
                Format.asprintf "%a" IO.pp_err reason)
            |> Bytestring.of_string
          in
          conn |> Conn.send_response `OK body
      | "multiple_body_read" :: [] ->
          Logger.debug (fun f -> f "multiple_body_read");
          Logger.debug (fun f ->
              f "multiple_body_read: %d" conn.req.body_remaining);
          let[@warning "-8"] (Conn.Ok (conn, body)) = Conn.read_body conn in
          Logger.debug (fun f ->
              f "multiple_body_read: %d" conn.req.body_remaining);
          let[@warning "-8"] (Conn.Error (conn, _reason)) =
            Conn.read_body conn
          in
          Logger.debug (fun f ->
              f "multiple_body_read: %d" conn.req.body_remaining);
          conn |> Conn.send_response `OK body
      | "expect_chunked_body" :: [] ->
          let transfer_encoding =
            Http.Header.get conn.req.headers "transfer-encoding" |> Option.get
          in
          let[@warning "-8"] (Conn.Ok (conn, actual_body)) =
            Conn.read_body conn
          in
          let actual_body = Bytestring.to_string actual_body in
          let expected_body =
            List.init 8_000_000 (fun _ -> "a") |> String.concat ""
          in
          Logger.debug (fun f -> f "actual_ %d" (String.length actual_body));
          assert (String.equal transfer_encoding "chunked");
          assert (String.equal actual_body expected_body);
          conn |> Conn.send_response `OK {%b|"OK"|}
      | [ "upgrade_websocket" ] -> conn |> Conn.upgrade (Obj.magic false)
      (* this is a confusing test, but the goal is to check if we fail to
         upgrade, we will return a 500 *)
      | [ "upgrade_unsupported" ] ->
          conn
          |> Conn.upgrade (Obj.magic false)
          |> Conn.send_response `OK {%b|"Not supported"|}
      | [ "date_header" ] ->
          conn
          |> Conn.with_header "date" "Tue, 27 Sep 2022 07:17:32 GMT"
          |> Conn.send_response `OK {%b|"OK"|}
      | _ -> failwith "not implemented"
    in

    let handler = Nomad.trail [ Trail.logger ~level:Debug (); hello_world ] in

    Nomad.start_link ~acceptors:1
      ~transport:
        Atacama.Transport.(
          tcp
            ~config:{ receive_timeout = 5_000_000L; send_timeout = 1_000_000L }
            ())
      ~config:
        (Nomad.Config.make ~max_header_count:40 ~max_header_length:5000 ())
      ~port:2112 ~handler ()
end

let () = Riot.start ~apps:[ (module Riot.Logger); (module Test) ] ()
