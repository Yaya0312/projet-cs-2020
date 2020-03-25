(lang dune 2.0.1)
(generate_opam_files true)

(name projet)
(authors "Pop")
(license MIT)

(package
 (name polynome)
 (synopsis "Permet de manipuler des polynomes ")
 (documentation "https://github.io/pop")
 (description "\
 Permet d'effectuer les opérations usuelles sur les polynomes")
 (depends
  (ocaml (>= 4.08.0))
  (alcotest (>=  1.0.1))
  (bisect_ppx ( >= 2.0.0))
  )
)

(package
 (name matrix)
 (synopsis "Permet de manipuler des matrices ")
 (documentation "https://github.io/pop")
 (description "\
 Permet d'effectuer quelques opérations sur les matrices")
)

(package
 (name bench)
 (synopsis "Permet d'obtenir des retours sur les fonctions ")
 (documentation "https://github.io/pop")
 (description "\
 Analyse les performances des fonctions")
)

(package
 (name gdnb)
 (synopsis "Permet de manipuler des grands nombres ")
 (documentation "https://github.io/pop")
 (description "\
 Permet d'effectuer les opérations usuelles sur les polynomes")
)