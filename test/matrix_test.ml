open Matrix
let m = [|[|1;2;3|];[|4;5;6|];[|7;8;9|]|]

(* TODO *)

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
