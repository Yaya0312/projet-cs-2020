(* -------------------------- Fonctions outils -------------------------------*)
exception SegmentNotExist;;

type abs = (int*int) list;;
type gdnb = {signe : bool; abs : (int*int) list};;

let sub_from_right (str:string) (size:int) (pos:int) =
  let start = (String.length str)-(size*(pos+1)) in
  if start < (-size) then raise SegmentNotExist else 
  let length = size+(min 0 start) in
  let start = max 0 start in
  String.sub str start length
;;
(** découpe la chaine str en segment de taille size et retourne le segment de
position pos. La segmentation de la chaine de caractère se fait de droite à
gauche. Le dernier segment peut avoir une taille comprise entre 1 et size. 
Les segments sont indicé a partir de 0*)

let make_string_from_ints (num1:int) (num2:int) (size:int) (carac:char) =
  let result_str = string_of_int (num1+num2) in
  let result_str_clean = (sub_from_right result_str size 0) in
  let blank_len = size-(String.length result_str_clean) in
  let blank = String.make blank_len carac in
  blank^result_str_clean
;;
(** retourne une chaine de caractere de taile size qui est l'addition des
deux nombres. La chaine de caractere resultante est completé par le caractère
carac. Si la somme des deux nombres depasse la taille size le la partie 
supérieure qui dépasse est tronqué *)

let rec get_abs acc abs1 = match abs1 with
  | [] -> 0
  | (a1, n1)::lst1 when a1=acc -> n1
  | (a1, n1)::lst1 when a1>acc -> 0 (* évite le parcours entier de la liste*)
  | (a1, n1)::lst1 -> get_abs acc lst1
;;
(** retourne le nombre associe à l'indice si l'indice n'existe pas retourne 0 *)

let rec sub_abs abs1 acc = match abs1 with
  | [] -> []
  | (a1, n1)::lst1 when a1>acc -> abs1
  | (a1, n1)::lst1 -> sub_abs lst1 acc
;;
(** retourne une partie du nombre en format absolue superieur à l'accumulateur
 *)

(* -------------------------- abs_of_string ----------------------------------*)
let abs_of_string (str:string) =
  let rec aux str acc result = 
  if (String.length str) <= (acc*4) then result
  else 
    let part_num = int_of_string(sub_from_right str 4 acc) in 
    if part_num = 0 then aux str (acc+1) result
    else aux str (acc+1) (result@[(acc,part_num)])
  in aux str 0 []
;;

(* -------------------------- string_of_abs ----------------------------------*)
let string_of_abs abs =
  let rec aux abs acc result = match abs with
  | (ind,num)::[] when ind = acc -> (string_of_int num)^result
  | (ind,num)::lst when ind = acc
    -> aux lst (acc+1) (make_string_from_ints 0 num 4 '0' )^result
  | _ 
    -> aux abs (acc+1) (make_string_from_ints 0 0 4 '0')^result
  in aux abs 0 ""
;;

(* -------------------------- gdnb_to_string ---------------------------------*)
let gdnb_to_string gdnb1 = 
  let signe = if gdnb1.signe then "-" else "" in
  signe^(string_of_abs gdnb1.abs)
;;

(* -------------------------- string_to_gdnb ---------------------------------*)
let string_to_gdnb str = 
  let signe = if (String.get str 0) = '-' then false else true in
  let string_abs = if signe then str
  else String.sub str 1 ((String.length str)-1) in
  let abs = abs_of_string string_abs in
  {signe= signe; abs=abs}
;;

(* --------------------------- compare_abs -----------------------------------*)
let compare_abs abs1 abs2 = 
  let rec aux abs1 abs2 = match abs1,abs2 with
  | [], [] -> true 
  | lst1, [] -> true
  | [], lst -> false
  | (a1,n1)::lst1, (a2,n2)::lst2 when (a1<>a2) -> a1>a2
  | (a1,n1)::lst1, (a2,n2)::lst2 when (n1<>n2) -> n1>n2
  | _::lst1, _::lst2  -> aux lst1 lst2
  in aux (List.rev abs1) (List.rev abs2)
;;
(** si abs1 >= abs2 return true sinon false *)

(* --------------------------- compare_gdnb ----------------------------------*)
let compare_gdnb gdnb1 gdnb2 = match gdnb1,gdnb2 with
  | e1,e2 when (e1.signe = e2.signe) 
    -> (compare_abs gdnb1.abs gdnb2.abs) && e1.signe
  | e1,e2 -> e1.signe
;;

(* --------------------------- additions -------------------------------------*)
let addition_abs (abs1:abs) (abs2:abs) =
  let rec aux (abs1:abs) (abs2:abs) (retain:int) (acc:int) (result:abs) = 
  let calcul = (get_abs acc abs1)+(get_abs acc abs2) + retain in
  let reste = calcul mod 10000 in
  let retain = calcul / 10000 in
  match abs1,abs2 with 
  | [], [] when retain<>0 -> (List.rev result)@[(acc,retain)]
  | [], [] -> (List.rev result)
  | [],_::lst | _::lst,[] -> (List.rev result)@[(acc,reste)]@lst
  | _ -> aux (sub_abs abs1 acc) (sub_abs abs2 acc) retain (acc+1) 
        ([(acc,reste)]@result)
  in aux abs1 abs2  0 0 []
;;

let substraction_abs (abs1:abs) (abs2:abs) =
  let rec aux (abs1:abs) (abs2:abs) (retain:int) (acc:int) (result:abs) = 
  let calcul = (get_abs acc abs1)-(get_abs acc abs2) - retain in
  let reste = calcul mod 10000 in
  let retain = calcul / 10000 in
  match abs1,abs2 with 
  | [], [] when retain<>0 -> (List.rev result)@[(acc,retain)]
  | [], [] -> (List.rev result)
  | [],_::lst | _::lst,[] -> (List.rev result)@[(acc,reste)]@lst
  | _ when reste = 0 ->  aux (sub_abs abs1 acc) (sub_abs abs2 acc) 
      retain (acc+1) result
  | _ -> aux (sub_abs abs1 acc) (sub_abs abs2 acc) retain (acc+1) 
        ([(acc,reste)]@result)
  in if (compare_abs abs1 abs2) then 
        aux abs1 abs2  0 0 []
      else
        aux abs2 abs1  0 0 []
;;

let addition_gndb (gdnb1:gdnb) (gdnb2:gdnb) = 
  if gdnb1.signe = gdnb2.signe then 
    (addition_abs gdnb1.abs gdnb2.abs)
  else
    (if gdnb1.signe = false then 
      substraction_abs gdnb2.abs gdnb1.abs
     else 
      substraction_abs gdnb2.abs gdnb1.abs
    )
;;

let (^^+) = addition_gndb;;
let (^^-) (g1:gdnb) (g2:gdnb) = 
    addition_gndb g1 {signe = not (g2.signe); abs = g2.abs}
;;

let (^^*) (g1:gdnb) (g2:gdnb) = match g1.abs, g2.abs with
  |([], _) | (_, []) -> []
  |([(0, 0)], _) | (_ ,[(0, 0)]) -> [(0, 0.)]
  |([(0, b)], p)  |(p, [(0, b)]) -> (p b)
  | _ -> 
  let k = (max (degre p1) (degre p2)) in (* To change *)
  let k = k + (k mod 2) in (* To change *)
  let mk = (k/2) in (* To change *)
  let p1_0,p1_1 = cut p1 mk in (* To change *)
  let p2_0,p2_1 = cut p2 mk in (* To change *)
  let c0 = p1_0 ^^* p2_0 in
  let c2 = p1_1 ^^* p2_1 in
  let u = (p1_0 ^^+ p1_1) ^^* (p2_0 ^^+ p2_1) in
  let c1 = (u ^^- c0) ^^- c2 in
  c0 ^^+ (c1 ^^ mk) ^+ (c2 ^^ k)    
;;