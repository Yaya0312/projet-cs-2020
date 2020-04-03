(** Module permettant la manipulation de polynome *)

(**  type polynome *)
type poly = (int*float) list;;

(** 
    [coef p i] retourne le coefficient de degrée i du polynome p \n
    Complexité O(n) n étant la longueur de la liste p \n
    recursive terminal && fonction pure
*)
val coef :  poly -> int -> float

(** 
    [p1 ^+ p2] retourne la somme des polynomes p1 et p2 \n
    Complexité O(n) n étant la longueur de la liste p1 ou p2 \n
    recursive terminal && fonction pure
*)
val (^+) : poly -> poly -> poly

(** 
    [multCoeff p c]
    Retourne le polynome p multiplié par le coef c
    Complexité O(n) n étant la longueur de la liste p
*)
val multCoeff : poly -> float -> poly

(** 
    [p1 ^- p2]
    Retourne la soustraction du polynome p1 par le polynome p2
    Complexité O(n) n étant la longueur de la liste p1
*)
val (^-) : poly -> poly -> poly

(** 
    [p1 ^= p2]
    Retourne vrai si les polynomes p1 et p2 sont égaux
    Complexité O(n) n étant la longueur de la liste p1
*)
val (^=) : poly -> poly -> bool

(** 
    [degre p]
    Retourne le degree du polynome p
    Complexité O(n) n étant la longueur de la liste p
*)
val degre : poly -> int

(** 
    [multXn p i]
    Retourne le polynome p multiplié par X^i
    Complexité O(n) n étant la longueur de la liste p
*)
val multXn : poly -> int -> poly

(** 
    [cut p i]
    Retourne le polynome p scindé (p0, p1) tel que p = p0 + (x^i) p1
    Complexité O(n) n étant la longueur de la liste p
*)
val cut : poly -> int -> poly * poly

(**
    [renverse k p]
    Retourne le renversé d'ordre k du polynome p
    Complexité O(n) n étant la longueur de la liste p
*)
val renverse : int -> poly -> poly

(** 
    [modulo p d]
    Retourne le reste de la division du polynome p par le monome de degre d
    Complexité O(n) n étant la longueur de la liste p
*)
val modulo : poly -> int -> poly

(** 
    [print_poly p]
    Affiche le polynome sur la sortie standard
    Complexité O(n) n étant la longueur de la liste p
*)
val print_poly : poly -> unit

(** 
    [horner p x]
    Retourne l'évaluation du polynome p avec la valeur x
    Complexité O(n) n étant la longueur de la liste p
*)
val horner : poly -> float -> float

(** 
    [random_poly deg coefmax]
    Retourne un polynome généré de degre maximal deg et de coef max maxcoef
    Complexité O(n) n étant le degré maximal possible du polynome
*)
val random_poly : int -> ?maxcoef:int -> unit -> poly

(** 
    [karatsuba p1 p2]
    Retourne p1 * p2 par la méthode de karatsuba (Toom Cook 2)
    TODO
*)
val karatsuba : poly -> poly -> poly

(** 
    [mult_naive p1 p2]
    Retourne p1 * p2 par la méthode naive
*)
val mult_naive : poly -> poly -> poly

(** 
    [toom_cook p1 p2]
    Retourne p1 * p2 par la méthode Toom cook 3
*)
val toom_cook3 : poly -> poly -> float -> poly