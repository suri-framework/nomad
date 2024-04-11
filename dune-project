(lang dune 3.12)

(name nomad)

(generate_opam_files true)

(source
 (github suri-framework/nomad))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name nomad)
 (synopsis "A Web server for Trail applications")
 (description "Nomad is a web server for the Riot scheduler written entirely in OCaml and is built atop the Atacama connection pool. It aims to be an Application-layer for Trail, implementing: HTTP/1.x, HTTP/2, and WebSockets. It is written with a big focus on clarity.")
 (depends 
   (atacama (>= "0.0.1"))
   (bitstring (>= "4.1.0"))
   (decompress (>= "1.5.3"))
   (digestif (>= "1.1.4"))
   (httpaf (>= "0.7.1"))
   (ocaml (>= "5.1.0"))
   (ppx_bitstring (>= "4.1.0"))
   (riot (>= "0.0.1"))
   (telemetry (>= "0.0.1"))
   (trail (>= "0.0.1"))
   (uutf (>= "1.0.3")))
 (tags
  (riot trail http https websocket ws web server suri)))
