(** Tools for grandNombres *)

type gdnb = {signe : bool; abs : (int*int) list};;
type abs = (int*int) list;;

val abs_of_string : string -> abs
(** convert a natural string number to natural number *)
(* exemple "12345" devient [(0,2345);(1,1)]*)

val string_of_abs : abs -> string
(** convert a natural number to natural string number *)
(* exemple [(0,2345);(1,1)] devient "12345"*)

val gdnb_to_string : gdnb -> string
(** convert gdnb to string *)

val string_to_gdnb :  string -> gdnb
(** convert string to gdnb *)

val addition_abs : abs  -> abs  -> abs 
(** permet d'aditioner deux grand nombres naturel*)

val soustraction_abs : abs  -> abs  -> abs 
(** permet la soustraction de deux grand nombres naturel*)

val addition_gdnb : gdnb -> gdnb -> gdnb
(** permet d'additioner deux grand nombres *)

val compare_gdnb : gdnb -> gdnb -> bool
(** retourne vrai si les grands nombres sont égaux sinon faux *)