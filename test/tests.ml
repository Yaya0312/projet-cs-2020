let test_suites: unit Alcotest.test list = [
  "coef", Polynome_test.coef_tests;
  "sum", Polynome_test.sum_tests;
  "^=", Polynome_test.equality_tests;
  "multCoeff", Polynome_test.multCoef_tests;
  "degree", Polynome_test.degree_tests;
  "multXn", Polynome_test.multXn_tests;
  "cut", Polynome_test.cut_tests;
  "mult_k", Polynome_test.mult_karatsuba_tests;
  "mult_n", Polynome_test.mult_naive_tests;
  (* "mult_t3", Polynome_test.mult_toom_cook3_tests; *)
  (* "renverse", Polynome_test.renverse_tests; *)
  "horner", Polynome_test.horner_tests;
  "get_row", Matrix_test.get_row_tests;
  "get_col", Matrix_test.get_col_tests
];;

let _ = Alcotest.run "Test" test_suites