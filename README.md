# nomad

Nomad is an HTTP server for [Trail][trail] apps inspired by [Bandit][bandit].

Nomad is written entirely in OCaml and is built atop [Atacama][atacama]. It
aims to be an Application-layer for Trail, implementing: HTTP/1.x, HTTP/2, and
WebSockets. It is written with a big focus on clarity.

[atacama]: https://github.com/suri-framework/atacama
[trail]: https://github.com/suri-framework/trail
[bandit]: https://github.com/mtrudel/bandit

## Correctness

Nomad aims to be **correct** and so we're testing against the [Bandit HTTP/1.1
test-bed][bandit-tests], [h2spec][h2spec], and [Autobahn][autobahn]

[bandit-tests]: ./test/bandit/test/bandit/http1/request_test.exs
[h2spec]: https://github.com/summerwind/h2spec
[autobahn]: https://github.com/crossbario/autobahn-testsuite

### HTTP/1.1

- [x] invalid requests
  - [x] returns a 400 if the request cannot be parsed
  - [x] returns a 400 if the request has an invalid http version
- [x] keepalive requests
  - [x] closes connection after max_requests is reached
  - [x] idle keepalive connections are closed after read_timeout
  - [x] unread content length bodies are read before starting a new request
  - [x] unread chunked bodies are read before starting a new request
- [x] origin-form request target (RFC9112§3.2.1)
  - [x] derives scheme from underlying transport
  - [x] derives host from host header
  - [x] returns 400 if no host header set in HTTP/1.1
  - [x] sets a blank host if no host header set in HTTP/1.0
  - [x] derives port from host header
  - [x] derives host from host header with ipv6 host
  - [x] derives host and port from host header with ipv6 host
  - [ ] returns 400 if port cannot be parsed from host header
  - [x] derives port from schema default if no port specified in host header
  - [x] derives port from schema default if no host header set in HTTP/1.0
  - [x] sets path and query string properly when no query string is present
  - [x] sets path and query string properly when query string is present
  - [x] ignores fragment when no query string is present
  - [x] ignores fragment when query string is present
  - [x] handles query strings with question mark characters in them
  - [x] returns 400 if a non-absolute path is send
  - [x] returns 400 if path has no leading slash
- [x] absolute-form request target (RFC9112§3.2.2)
  - [x] uses request-line scheme even if it does not match the transport
  - [x] derives host from the URI, even if it differs from host header
  - [x] derives ipv6 host from the URI, even if it differs from host header
  - [x] does not require a host header set in HTTP/1.1 (RFC9112§3.2.2)
  - [x] derives port from the URI, even if it differs from host header
  - [x] derives port from schema default if no port specified in the URI
  - [x] sets path and query string properly when no query string is present
  - [x] sets path and query string properly when query string is present
  - [x] ignores fragment when no query string is present
  - [x] ignores fragment when query string is present
  - [x] handles query strings with question mark characters in them
- [x] authority-form request target (RFC9112§3.2.3)
  - [x] returns 400 for authority-form / CONNECT requests
- [ ] asterisk-form request target (RFC9112§3.2.4)
  - [ ] parse global OPTIONS path correctly
- [ ] request line limits
  - [ ] returns 414 for request lines that are too long
- [ ] request headers
  - [ ] reads headers properly
  - [ ] returns 431 for header lines that are too long
  - [ ] returns 431 for too many header lines
- [ ] content-length request bodies
  - [ ] reads a zero length body properly
  - [ ] reads a content-length encoded body properly
  - [ ] reads a content-length with multiple content-lengths encoded body properly
  - [ ] rejects a request with non-matching multiple content lengths
  - [ ] rejects a request with negative content-length
  - [ ] rejects a request with non-integer content length
  - [ ] handles the case where we ask for less than is already in the buffer
  - [ ] handles the case where we ask for more than is already in the buffer
  - [ ] handles the case where we read from the network in smaller chunks than we return
  - [ ] handles the case where the declared content length is longer than what is sent
  - [ ] handles the case where the declared content length is less than what is sent
  - [ ] reading request body multiple times works as expected
- [ ] chunked request bodies
  - [ ] reads a chunked body properly
- [ ] upgrade handling
  - [ ] raises an ArgumentError on unsupported upgrades
  - [ ] returns a 400 and errors loudly in cases where an upgrade is indicated but the connection is not a GET
  - [ ] returns a 400 and errors loudly in cases where an upgrade is indicated but upgrade header is incorrect
  - [ ] returns a 400 and errors loudly in cases where an upgrade is indicated but connection header is incorrect
  - [ ] returns a 400 and errors loudly in cases where an upgrade is indicated but key header is incorrect
  - [ ] returns a 400 and errors loudly in cases where an upgrade is indicated but version header is incorrect
  - [ ] returns a 400 and errors loudly if websocket support is not enabled
- [ ] response headers
  - [ ] writes out a response with a valid date header
  - [ ] returns user-defined date header instead of internal version
- [ ] response body
  - [x] writes out a response with deflate encoding if so negotiated
  - [x] writes out a response with gzip encoding if so negotiated
  - [ ] writes out a response with x-gzip encoding if so negotiated
  - [x] uses the first matching encoding in accept-encoding
  - [x] falls back to no encoding if no encodings provided
  - [x] does no encoding if content-encoding header already present in response
  - [x] does no encoding if a strong etag is present in the response
  - [x] does content encoding if a weak etag is present in the response
  - [x] does no encoding if cache-control: no-transform is present in the response
  - [x] falls back to no encoding if no encodings match
  - [ ] falls back to no encoding if compression is disabled
  - [x] sends expected content-length but no body for HEAD requests
  - [x] replaces any incorrect provided content-length headers
  - [x] writes out a response with no content-length header or body for 204 responses
  - [x] writes out a response with no content-length header or body for 304 responses
  - [x] writes out a response with zero content-length for 200 responses
  - [x] writes out a response with zero content-length for 301 responses
  - [x] writes out a response with zero content-length for 401 responses
  - [x] writes out a chunked response
  - [x] does not write out a body for a chunked response to a HEAD request
  - [x] returns socket errors on chunk calls
  - [x] writes out a sent file for the entire file with content length
  - [x] writes out headers but not body for files requested via HEAD request
  - [x] does not write out a content-length header or body for files on a 204
  - [x] does not write out a content-length header or body for files on a 304
  - [x] writes out a sent file for parts of a file with content length
- [x] sending informational responses
- [x] does not send informational responses to HTTP/1.0 clients
- [x] reading HTTP version
- [ ] reading peer data

### HTTP/2

### WebSockets
