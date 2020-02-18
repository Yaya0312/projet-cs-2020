let test_suites: unit Alcotest.test list = [
  "coef", Polynome_test.coef_tests;
  "sum", Polynome_test.sum_tests;
  "multCoeff", Polynome_test.multCoef_test;
  "degree", Polynome_test.degree_tests;
  "multXn", Polynome_test.multXn_tests;
  "cut", Polynome_test.cut_tests;
  "mult", Polynome_test.mult_tests;
  "renverse", Polynome_test.renverse_tests;
  "horner", Polynome_test.horner_tests;
];;

let _ = Alcotest.run "Polynomes" test_suites