exception MaxLowerThanMin;;

(** Interverti les valeurs a[i] a[j] *)
let swap (a:'a array) (i:int) (j:int) =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
;;

(** Mélange le tableau Complexité O(n) n étant la longueur du tableau *)
let shuffle a =
    Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a
;;

(** Génére un tableau de min à max *)
let make_array min max =
    let r = max - min in
    if r < 0 then raise MaxLowerThanMin else 
    Array.init (r+1) (fun i -> (i+min) );;
;;

(** Réalise l'ensemble répondant à la question 1 *)
let make_custom_array = 
    let a1 = make_array 0 10 in
    let a2 = make_array 11 10000 in
    shuffle a2;
    let a2 = Array.sub a2 0 ((1000 - Array.length a1)) in
    Array.append a1 a2
;;

(** Affiche sur la sortie standard le temps d'execution moyen pour les différents algo pour le deg d *)
let print_time_mult ht deg =
    let (k,t,p)= (Hashtbl.find ht deg) in
    let sentences = "Temps d'éxcution moyen pour le deg "^ deg ^ "\n" in
    let karatsuba = "karatsuba" ^ (string_of_float k) ^ "\n" in
    let tom_cook = "toom3" ^ (string_of_float t) ^ "\n" in
    print_string (sentences ^ karatsuba ^ tom_cook)
;;

let create_array_polynome num deg = 
    Array.init (num + 1) (fun _ -> random_poly deg max_float);;
;;

(** Permet de generer la table de hachage contenant les 10 polynomes pour chaques degre de l'ensemble set *)
let make_hashtable set h =
    Array.iter (fun a -> Hashtbl.add h a (create_array_polynome 10 a)) set
;;

(** Pioche deux element dans la liste *)
let pick (a:a' array) =
    let random1 = random.int (Array.length a) and
        random2 = random.int (Array.length a) in
    Array.get a random1, Array.get a random2
;;

(** Retourne le temps que la fonction à mis pour s'executer *)
let time_fun func =
  let start_time = Sys.time () in
  ignore func;
  let finish_time = Sys.time () in
  finish_time -. start_time
;;

(* Effectue l'operation plusieurs fois avoir une moyenne *)
let average_time func num = 
    (time_fun func)/. num
;;

let run k v = 
    let p1,p2 = pick v in
    let tk = average_time karatsuba in
    let tt = average_time tom_cook in 
    Hashtbl.add k [tk, tt, ...] 
;; 


(* On effectue 45 fois l'operation pour chaque multiplication *)
let make_time_ht = let Hashtbl.iter run ;;
