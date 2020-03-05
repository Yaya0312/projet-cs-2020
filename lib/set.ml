exception MaxLowerThanMin;;
(** TOOLS *)

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
    Array.init (r + 1) (fun i -> (i + min) );;
;;

(** END TOOLS *)

(** Affiche sur la sortie standard le temps d'execution moyen pour les 
        différents algo pour le deg d *)
let print_time_mult ht deg =
    let (k,t,p)= (Hashtbl.find ht deg) in
    let sentences = "Temps d'éxcution moyen pour le deg " ^ 
    (string_of_int deg) ^ "\n" in
    let karatsuba = "Karatsuba " ^ (string_of_float k) ^ " sec \n" in
    let tom_cook = "Toom 3 " ^ (string_of_float t) ^ " sec \n" in
    print_string (sentences ^ karatsuba ^ tom_cook)
;;

(** creation d'un  *)
let create_array_polynome num deg = 
    Array.init (num + 1) (fun _ -> random_poly deg max_float);;
;;

(** Permet de generer la table de hachage contenant les 10 polynomes pour chaques degre de l'ensemble set *)
let make_hashtable set h =
    Array.iter (fun a -> Hashtbl.add h a (create_array_polynome 10 a)) set
;;

(** Pioche deux element dans la liste *)
let pick a =
    let random1 = Random.int (Array.length a) and
        random2 = Random.int (Array.length a) in
    Array.get a random1, Array.get a random2
;;

(** Retourne le temps que la fonction à mis pour s'executer *)
let time_fun func arg =
  let start_time = Sys.time () in
  ignore (func arg);
  let finish_time = Sys.time () in
  finish_time -. start_time
;;

time_fun (fun a -> random_poly (fst a) (snd a)) (20000000,1000.);;

let run k v = 
    let p1,p2 = pick v in
    let tk = (time_fun (fun a -> (^*) (fst a) (snd a)) (p1,p2)) in

    Hashtbl.add time_table k tk (* TODO (tk) -> (tk, tt ,tn) *)
;;

let main =
    let a1 = make_array 0 10 in
    let a2 = make_array 11 5000 in (* TODO remplacer par 10000 *)
    shuffle a2;
    let a2 = Array.sub a2 0 ((1000 - Array.length a1)) in
    Array.append a1 a2;


    Hashtbl.iter run
;;