(lang dune 2.0.1)
(generate_opam_files true)

(name projet)
(authors "Pop")
(license MIT)
(maintainers "Pop")
(source (github Yaya0312/projet-cs-2020))
(homepage https://github.com/Yaya0312/projet-cs-2020)
(bug_reports https://github.com/Yaya0312/projet-cs-2020/issues)
(documentation https://yaya0312.github.io/projet-cs-2020/)


(package
 (name polynome)
 (synopsis "Permet de manipuler des polynomes ")
 (description "\
 Permet d'effectuer les opérations usuelles sur les polynomes")
 (depends
  (ocaml (>= 4.05.0))
  (alcotest (>=  1.0.1))
  (bisect_ppx ( >= 2.0.0))
  (odoc ( >= 1.5.0))
  )
)

(package
 (name matrix)
 (synopsis "Permet de manipuler des matrices ")
 (description "\
 Permet d'effectuer quelques opérations sur les matrices")
)

(package
 (name benchTools)
 (synopsis "Permet d'obtenir des retours sur les fonctions ")
 (description "\
 Analyse les performances des fonctions")
)

(package
 (name gdnb)
 (synopsis "Permet de manipuler des grands nombres ")
 (description "\
 Permet d'effectuer les opérations usuelles sur les polynomes")
)