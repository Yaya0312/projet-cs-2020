exception MaxLowerThanMin;;

val swap : 'a array -> int -> int -> unit
(** 
    Interverti les valeurs a[i] a[j] du tableau
    Complexité O(1)
*)

val shuffle : 'a array -> unit
(** 
    Mélange le tableau
    Complexité O(n)
*)

val make_array : int -> int -> unit
(** Génére un tableau de min à max *)
