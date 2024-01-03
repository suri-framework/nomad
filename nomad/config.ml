type t = {
  max_line_request_length : int;
  max_header_count : int;
  max_header_length : int;
  request_receive_timeout : int64;
}

let make ?(max_line_request_length = 8000) ?(max_header_count = 100)
    ?(max_header_length = 5000) ?(request_receive_timeout = 1_000_000L) () =
  {
    max_line_request_length;
    max_header_count;
    max_header_length;
    request_receive_timeout;
  }
