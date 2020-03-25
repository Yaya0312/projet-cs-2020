exception MaxLowerThanMin;;

val swap : 'a array -> int -> int -> unit
(** 
    Interverti les valeurs a[i] a[j] du tableau
    Complexité O(1)
*)

val shuffle : 'a array -> unit
(** 
    Mélange le tableau
    Complexité O(n) -  Parcours le tableau de longueur n
*)

val make_array : int -> int -> int array
(** 
    Génére un tableau de min à max (longueur min - max + 1)
    Complexity O(n) - Initialise le tableau de longueur n
*)
