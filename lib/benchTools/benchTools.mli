(** 
    [swap a i j] Interverti les valeurs a[i] a[j] du tableau a \n
    Complexité O(1)
*)
val swap : 'a array -> int -> int -> unit

(** 
    [shuffle a start len] Mélange len éléments du tableau a à partir de start \n
    Complexité O(len-start)
*)
val shuffle : 'a array -> ?start:int -> ?len:int -> unit -> unit

(** 
    [make_array min max] Génére un tableau de min à max (longueur min - max + 1)
    Complexity O(n) - Initialise le tableau de longueur n
*)
val make_array : int -> int -> int array

val calc: int -> float array

val export_latex : (int, float array) Hashtbl.t -> string -> unit