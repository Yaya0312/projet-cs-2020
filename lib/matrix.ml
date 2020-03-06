
type matrix = (float) array array;;

(* demo *)
let a = [| 
[|1.;2.;3.|];
[|4.;5.;6.|];
[|7.;8.;9.|]
|];;

(** Retourne la ligne row de la matrice m *)
let get_row (m:matrix) (row:int) = 
    Array.get a row
;;

(** Retourne la colonne col de la matrice m *)
let get_col (m:matrix) (col:int) = 
    Array.map (fun m -> Array.get m col) m
;;

let max_col_from_diag (m:matrix) (numcol:int) = 
    let function_a acc b = 
        let acc = max  b in
        let b = snd  in
        (a,b) in
    Array.fold_left function_a (min_float,0,0) (m.numcol)
;;

