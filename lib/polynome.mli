(** Module permettant la manipulation de polynome *)

(**  type polynome *)
type poly = (int*float) list;;

(** Retourne le coefficient de degrée i du polynome p *)
val coef :  poly -> int -> float

(** Retourne la somme des polynomes p1 et p2 *)
val (^+) : poly -> poly -> poly

(** Retourne le polynome p multiplié par le coef c *)
val multCoeff : poly -> float -> poly

(** Retourne la soustraction du polynome p1 par le polynome p2 *)
val (^-) : poly -> poly -> poly

(** Retourne vrai si les polynomes p1 et p2 sont égaux *)
val (^=) : poly -> poly -> bool

(** Retourne le degree du polynome p *)
val degre : poly -> int

(** Retourne le polynome p multiplié par X^i *)
val multXn : poly -> int -> poly

(** Retourne le polynome p scindé (p0, p1) tel que p = p0 + (x^i) p1 *)
val cut : poly -> int -> poly * poly

(** Retourne la multiplication des polynomes p1 et p2 *)
val (^*) : poly -> poly -> poly

(** Retourne le renversé d'ordre k du polynome p *)
val renverse : int -> poly -> poly

(** Retourne le reste de la division du polynome p par le monome de degre d *)
val modulo : poly -> int -> poly

(** Affiche le polynome sur la sortie standard *)
val print_poly : poly -> unit

(** Retourne l'évaluation du polynome p avec la valeur x *)
val horner : poly -> float -> float

(** Retourne un polynome généré de degre maximal deg et de coef max maxcoef *)
val random_poly : int -> int -> poly