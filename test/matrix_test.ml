open Matrix
let m = [|[|1.;2.;3.|];[|4.;5.;6.|];[|7.;8.;9.|]|];;


(*** get_row ******************************************************************)

let test_get_row  = Alcotest.check (Alcotest.array (Alcotest.float 0.)) "get_row";;

let test_get_row_0 () = test_get_row [|1.;2.;3.|] (get_row m 0) ;;

let get_row_tests =
  [ 
    ("coef with negative deg p1", `Quick, test_get_row_0);
  ]
;;