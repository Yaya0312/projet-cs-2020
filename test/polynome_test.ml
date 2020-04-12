open Polynome
let p1 = [(0,2.);(1,-1.);(3,5.)];;
let p2 = [(0,1.);(1,-1.);(2,1.);(3,1.);(4,2.)];;
let alcotestPoly = Alcotest.(list (pair int (float 0.0001)));;

(*** Coeff ********************************************************************)

let test_coef  = Alcotest.check (Alcotest.float 0.) "coef";;

let test_coef_neg_p1 () = test_coef 0. (coef p1 (-1)) ;;
let test_coef_in_p1  () = test_coef 2. (coef p1 0) ;;
let test_coef_greater_p1 () = test_coef 0. (coef p1 8) ;;
let test_coef_neg_p2 () = test_coef 0. (coef p2 (-1)) ;;
let test_coef_in_p2 () = test_coef 1. (coef p2 3) ;;
let test_coef_greater_p2 () = test_coef 0. (coef p2 8) ;;

let coef_tests =
  [ 
    ("coef with negative deg p1", `Quick, test_coef_neg_p1);
    ("coef with in deg p1", `Quick, test_coef_in_p1);
    ("coef with greater deg p1", `Quick, test_coef_greater_p1);
    ("coef with negative deg p2", `Quick, test_coef_neg_p1);
    ("coef with in deg p2", `Quick, test_coef_in_p1);
    ("coef with greater deg p2", `Quick, test_coef_greater_p1);
  ];;

(*** Sum **********************************************************************)
let test_sum = Alcotest.check alcotestPoly "sum";;

let test_sum_p1_p2 () = test_sum  (p1 ^+ p2)  [(0,3.);(1,-2.);(2,1.);(3,6.);(4,2.)];;
let test_sum_p1_empty () = test_sum (p1 ^+ []) [(0,2.);(1,-1.);(3,5.)];;

let sum_tests =
  [ 
    ("p1 + p2", `Quick, test_sum_p1_p2);
    ("p1 + []", `Quick, test_sum_p1_empty);
  ];;

(*** ^= ***********************************************************************)
let test_equality = Alcotest.check Alcotest.bool "equality";;

let test_equality_p1_p1 () = test_equality true (p1 ^= p1);;
let test_equality_p2_p2 () = test_equality true (p2 ^= p2);;
let test_equality_p1_p2 () = test_equality false (p1 ^= p2);;
let test_equality_p2_p1 () = test_equality false (p2 ^= p1);;
let test_equality_empty_p1 () = test_equality false ([] ^= p1);;

let equality_tests =
  [
    ("p1 ^= p1", `Quick, test_equality_p1_p1);
    ("p2 ^= p2", `Quick, test_equality_p2_p2);
    ("p1 ^= p2", `Quick, test_equality_p1_p2);
    ("p1 ^= p2", `Quick, test_equality_p2_p1);
    ("[] ^= p1", `Quick, test_equality_empty_p1);
  ];;

(*** multCoeff ****************************************************************)

let test_multCoef  = Alcotest.check alcotestPoly "coef";;

let test_multCoef_0 () = test_multCoef [] (multCoeff p1 0.);;
let test_multCoef_2 () = test_multCoef [(0,4.);(1,-2.);(3,10.)] (multCoeff p1 2.);;
let test_multCoef_neg2 () = test_multCoef [(0,-4.);(1,2.);(3,-10.)] (multCoeff p1 (-2.));;

let multCoef_tests =
  [
    ("multCoef with 0", `Quick, test_multCoef_0);
    ("multCoef with 2", `Quick, test_multCoef_2);
    ("multCoef with -2", `Quick, test_multCoef_neg2);
  ];;

(*** degree *******************************************************************)

let test_degree = Alcotest.check Alcotest.int "degree"

let test_degree_empty () = test_degree (-1) (degre []);;
let test_degree_p1 () = test_degree 3 (degre p1);;
let test_degree_p2 () = test_degree 4 (degre p2);;

let degree_tests =
  [ 
    ("degree empty", `Quick, test_degree_empty);
    ("degree p1", `Quick, test_degree_p1);
    ("degree p2", `Quick, test_degree_p2);
  ];;

(*** multXn *******************************************************************)

let test_multXn = Alcotest.check alcotestPoly "multXn";;

let test_multXn_p1_0 () = test_multXn p1 (multXn p1 0);;
let test_multXn_p1_1 () = test_multXn [(1,2.);(2,-1.);(4,5.)] (multXn p1 1);;

let multXn_tests =
  [ 
    ("multXn p1 with 0", `Quick, test_multXn_p1_0);
    ("multXn p1 with 1", `Quick, test_multXn_p1_1);
  ];;

(*** cut **********************************************************************)

let test_cut = Alcotest.check (Alcotest.pair alcotestPoly alcotestPoly) "multXn";;

let test_cut_p1_neg1 () = test_cut ([],[(1,2.);(2,-1.);(4,5.)]) (cut p1 (-1));;
let test_cut_p1_1 () = test_cut  ([(0,2.)],[(0,-1.);(2,5.)]) (cut p1 1);;
let test_cut_p1_2 () = test_cut  ([(0,2.);(1,-1.)],[(1,5.)]) (cut p1 2);;
let test_cut_p1_5 () = test_cut  ([(0,2.);(1,-1.);(3,5.)],[]) (cut p1 5);;

let cut_tests =
  [
    ("cut p1 with -1", `Quick, test_cut_p1_neg1);
    ("cut p1 with 1", `Quick, test_cut_p1_1);
    ("cut p1 with 2", `Quick, test_cut_p1_2);
    ("cut p1 with 5", `Quick, test_cut_p1_5);
  ];;

(*** mult result **************************************************************)
let r1 =[(0, 2.); (1, (-3.)); (2, 3.); (3, 6.); (4, (-2.)); (5, 3.); (6, 5.); (7, 10.)];;
let r2 =[(0, 4.); (1, -4.); (2, 1.); (3, 20.); (4, -10.); (6, 25.)];;
let r3 =[(0, 1.); (1, -2.); (2, 3.); (4, 3.); (5, -2.); (6, 5.); (7, 4.); (8, 4.)];;

(*** mult karatsuba ***********************************************************)

let test_mult_k = Alcotest.check alcotestPoly "multiplication karatsuba";;
let test_mult_k_p1_p2 () = test_mult_k r1 (karatsuba p1 p2);;
let test_mult_k_p1_p1 () = test_mult_k r2 (karatsuba p1 p1);;
let test_mult_k_p2_p2 () = test_mult_k r3 (karatsuba p2 p2);;
let test_mult_k_p1_empty () = test_mult_k [] (karatsuba p1 []);;
let test_mult_k_empty_p1 () = test_mult_k [] (karatsuba [] p1);;
let test_mult_k_p2_empty () = test_mult_k [] (karatsuba p2 []);;
let test_mult_k_empty_p2 () = test_mult_k [] (karatsuba [] p2);;

let mult_karatsuba_tests =
  [
    (" p1 * p2", `Quick, test_mult_k_p1_p2);
    (" p1 * p1", `Quick, test_mult_k_p1_p1);
    (" p2 * p2", `Quick, test_mult_k_p2_p2);
    (" p1 * []", `Quick, test_mult_k_p1_empty);
    (" [] * p1", `Quick, test_mult_k_empty_p1);
    (" p2 * []", `Quick, test_mult_k_p2_empty);
    (" [] * p2", `Quick, test_mult_k_empty_p2);
  ];;

(*** mult naive ***********************************************************)

let test_mult_n = Alcotest.check alcotestPoly "multiplication naive";;
let test_mult_n_p1_p2 () = test_mult_n r1 (mult_naive p1 p2);;
let test_mult_n_p1_p1 () = test_mult_n r2 (mult_naive p1 p1);;
let test_mult_n_p2_p2 () = test_mult_n r3 (mult_naive p2 p2);;
let test_mult_n_p1_empty () = test_mult_n [] (mult_naive p1 []);;
let test_mult_n_empty_p1 () = test_mult_n [] (mult_naive [] p1);;
let test_mult_n_p2_empty () = test_mult_n [] (mult_naive p2 []);;
let test_mult_n_empty_p2 () = test_mult_n [] (mult_naive [] p2);;

let mult_naive_tests =
  [
    (" p1 * p2", `Quick, test_mult_n_p1_p2);
    (" p1 * p1", `Quick, test_mult_n_p1_p1);
    (" p2 * p2", `Quick, test_mult_n_p2_p2);
    (" p1 * []", `Quick, test_mult_n_p1_empty);
    (" [] * p1", `Quick, test_mult_n_empty_p1);
    (" p2 * []", `Quick, test_mult_n_p2_empty);
    (" [] * p2", `Quick, test_mult_n_empty_p2);
  ];;

(*** mult toom_cook3 ***********************************************************)

let test_mult_t3 = Alcotest.check alcotestPoly "multiplication toom_cook3";;
let test_mult_t3_p1_p2 () = test_mult_t3 r1 (toom_cook3 p1 p2 0.1);;
let test_mult_t3_p1_p1 () = test_mult_t3 r2 (toom_cook3 p1 p1 0.1);;
let test_mult_t3_p2_p2 () = test_mult_t3 r3 (toom_cook3 p2 p2 0.1);;
let test_mult_t3_p1_empty () = test_mult_t3 [] (toom_cook3 p1 [] 0.1);;
let test_mult_t3_empty_p1 () = test_mult_t3 [] (toom_cook3 [] p1 0.1);;
let test_mult_t3_p2_empty () = test_mult_t3 [] (toom_cook3 p2 [] 0.1);;
let test_mult_t3_empty_p2 () = test_mult_t3 [] (toom_cook3 [] p2 0.1);;

let mult_toom_cook3_tests =
  [
    (" p1 * p2", `Quick, test_mult_t3_p1_p2);
    (" p1 * p1", `Quick, test_mult_t3_p1_p1);
    (" p2 * p2", `Quick, test_mult_t3_p2_p2);
    (" p1 * []", `Quick, test_mult_t3_p1_empty);
    (" [] * p1", `Quick, test_mult_t3_empty_p1);
    (" p2 * []", `Quick, test_mult_t3_p2_empty);
    (" [] * p2", `Quick, test_mult_t3_empty_p2);
  ];;


(*** renverse *****************************************************************)
let test_renverse = Alcotest.check alcotestPoly "renverse";;

let test_renverse_2_p1 () = test_renverse [(-1,5.);(1,-1.);(2,2.)] (renverse 2 p1);;

let renverse_tests = 
  [
    (" renv p1 with 2", `Quick, test_renverse_2_p1);
  ];;

(*** modulo *******************************************************************)
(* let test_modulo = Alcotest.check alcotestPoly "modulo";;

   let test_modulo_2_p1 () = test_modulo [(-1,5.);(1,-1.);(2,2.)] (modulo 2 p1);;

   let modulo_modulo = 
   [
     (" renv p1 with 2", `Quick, test_modulo_2_p1);
   ];; *)


(*** horner *******************************************************************)
let test_horner = Alcotest.check  (Alcotest.float 0.1) "horner";;

let test_horner_p1_0 () = test_horner 2. (horner p1 0.) ;;
let test_horner_p1_2 () = test_horner 40. (horner p1 2.) ;;

let horner_tests = 
  [
    (" horner p1 with 0.", `Quick, test_horner_p1_0);
    (" horner p1 with 2.", `Quick, test_horner_p1_2);
  ];;
