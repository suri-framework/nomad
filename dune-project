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
   (ocaml (>= "5.0.0"))
   dune
   riot
   telemetry
   caravan
   (http (>= "6.0.0~alpha2"))
   (bitstring (>= "4.1.0"))
   (ppx_bitstring (>= "4.1.0"))
   )
 (tags
  (riot http server)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
