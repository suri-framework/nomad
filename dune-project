(lang dune 3.10)

(name nomad)

(generate_opam_files true)

(source
 (github leostera/nomad))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name nomad)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (atacama (>= "0.0.4"))
   (bitstring (>= "4.1.0"))
   (decompress (>= "1.5.3"))
   (digestif (>= "1.1.4"))
   (httpaf (>= "0.7.1"))
   (ocaml (>= "5.1.0"))
   (ppx_bitstring (>= "4.1.0"))
   (riot (>= "0.0.7"))
   (telemetry (>= "0.0.1"))
   (trail (>= "0.0.1"))
   (uutf (>= "1.0.3")))
 (tags
  (riot http server)))
