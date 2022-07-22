(lang dune 3.3)

(name drunner)

(generate_opam_files true)

(source
 (github mark-beck/drunner))

(authors "Mark Beck")

(maintainers "Mark Beck")

(license License)

(package
 (name drunner)
 (synopsis "A dmenu based application launcher")
 (description "A dmenu based application launcher")
 (depends ocaml dune)
 (tags
  ("dmenu" "runner" "app launcher")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
