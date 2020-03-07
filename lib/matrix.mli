type matrix

(** 
    [get_row m i] Retourne la ligne i de la matrice m 
    Complexity O(1)
*)
val get_row : matrix -> int -> float array (** TODO replace par 'a *)

(** 
    [get_col m i] Retourne la colone i de la matrice m
    Complexity O(n) n étant le nombre de lignes
*)
val get_col : matrix -> int -> float array (** TODO replace par 'a *)

(** 
    [get_diag m] Retourne la diagonale de la matrice carré m
    Complexity O(n) n étant le nombre de lignes
*)
val get_diag : matrix -> float array (** TODO replace par 'a *)

(** 
    [mul_row m row coef] Multiplie la ligne row de la matrice m par coeff
    Complexity O(n) TODO
*)
val mul_row : matrix -> int -> float -> unit

(** 
    [sum_row m num_row row] Additionne la ligne num_row de la matrice m avec
    la ligne row
    Complexity O(n) TODO
*)
val sum_row : matrix -> int -> 'a array -> unit

(** 
    [swap_row m row1 row2] Échange les lignes row1 et row2 de la matrice m
    Complexity O(n) TODO
*)
val swap_row : matrix -> int -> int -> unit