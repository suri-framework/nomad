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
   (ocaml (>= "5.1.0"))
   (httpaf (>= "0.7.1"))
   (riot (>= "0.0.7"))
   (telemetry (>= "0.0.1"))
   (atacama (>= "0.0.4"))
   (http (>= "6.0.0~alpha2"))
   (bitstring (>= "4.1.0"))
   (ppx_bitstring (>= "4.1.0"))
   )
 (tags
  (riot http server)))
