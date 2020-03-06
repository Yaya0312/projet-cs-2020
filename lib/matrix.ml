
type matrix = (a') array array;;

(** Retourne la ligne row de la matrice m *)
let get_row (m:matrix) (row:int) : ('a array) = 
    Array.get m row
;;

(** Retourne la colonne col de la matrice m *)
let get_col (m:matrix) (col:int) = 
    Array.map (fun m -> Array.get m col) m
;;