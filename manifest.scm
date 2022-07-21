(use-modules
 (guix packages)
 (gnu packages ocaml)
 (feser ocaml))

(packages->manifest
 (list dune
       ocaml
       ocaml-base
       ocaml-core-unix
       ocaml-core-bench
       ocaml-fmt
       ocaml-base-quickcheck
       ocaml-ppx-jane

       ocaml-lsp-server
       ocamlformat))
