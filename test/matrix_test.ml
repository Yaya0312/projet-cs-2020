open Matrix
let m = [|[|1.;2.;3.|];[|4.;5.;6.|];[|7.;8.;9.|]|];;

(*** get_row ******************************************************************)
let test_get_row = Alcotest.check (Alcotest.(array (float 0.))) "get_row";;

let test_get_row_1_m () =  test_get_row [|1.;2.;3.|] (get_row m 0);;
let test_get_row_2_m () =  test_get_row [|4.;5.;6.|] (get_row m 1);;
let test_get_row_3_m () =  test_get_row [|7.;8.;9.|] (get_row m 2);;

let get_row_tests =
  [
    ("get_row 1", `Quick, test_get_row_1_m);
    ("get_row 2", `Quick, test_get_row_2_m);
    ("get_row 3", `Quick, test_get_row_3_m);
  ]
;;

(*** get_row ******************************************************************)
let test_get_col = Alcotest.check (Alcotest.(array (float 0.))) "get_col";;

let test_get_col_1_m () =  test_get_col [|1.;4.;7.|] (get_col m 0);;
let test_get_col_2_m () =  test_get_col [|2.;5.;8.|] (get_col m 1);;
let test_get_col_3_m () =  test_get_col [|3.;6.;9.|] (get_col m 2);;

let get_col_tests =
  [
    ("get_col 1", `Quick, test_get_col_1_m);
    ("get_col 2", `Quick, test_get_col_2_m);
    ("get_col 3", `Quick, test_get_col_3_m);
  ];;
