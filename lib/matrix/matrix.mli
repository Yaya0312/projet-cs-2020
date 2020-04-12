type 'a matrix = 'a array array;;

(** 
    [get_el m col row]
    Retourne l'élement m_(i,j) 
    Complexity O(1)
*)
val get_el : 'a matrix -> int -> int -> 'a


(** 
    [get_row m i]
    Retourne la ligne i de la matrice m 
    Complexity O(1)
*)
val get_row : 'a matrix -> int -> 'a array

(** 
    [get_col m i]
    Retourne la colone i de la matrice m
    Complexity O(n) n étant le nombre de lignes
*)
val get_col : 'a matrix -> int -> 'a array

(** 
    [mul_row row coef]
    Retourne la ligne row multiplié par coef
    Complexity O(n) TODO
*)
(*val mul_row : float array -> float -> float array*)

(** 
    [sum_row row1 row2]
    Retourne la somme des tableaux (termes à termes)
    Complexity O(n) TODO
*)
val sum_row : float array -> float array -> float array

(** 
    [swap_row m row1 row2]
    Échange les lignes row1 et row2 de la matrice m
    Complexity O(n) TODO
*)
val swap_row : 'a matrix -> int -> int -> unit

(**
    [max_pos_array a ]
    Retourne la position ou le maximum est present dans le tableau
    Complexity O(n) TODO
*)
val max_pos_array : float array -> int -> int

(**
    [solve m row]
    Retourne l'ensemble des solutions en utilisant l'algorithme de gauss
    Complexity ??
*)
val solve : float matrix -> float array