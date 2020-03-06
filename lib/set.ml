exception MaxLowerThanMin;;
(** TOOLS *)

(** Interverti les valeurs a[i] a[j] *)
let swap (a:'a array) (i:int) (j:int) : unit =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
;;

(** Mélange le tableau Complexité O(n) n étant la longueur du tableau *)
let shuffle (a:'a array) : unit =
    Array.iteri (fun i _ -> swap a i (Random.int (i + 1))) a
;;

(** Génére un tableau de min à max *)
let make_array (min:int) (max:int) : int array =
    let r = max - min in
    if r < 0 then raise MaxLowerThanMin else 
    Array.init (r + 1) (fun i -> (i + min))
;;

(** END TOOLS *)

(** creation d'un  *)
let create_array_polynome (num:int) (deg:int) : int array = 
    Array.init (num + 1) (fun _ -> random_poly deg max_float);;
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


let run (k:int) v = 
    let p1,p2 = pick (fst v) in
    let tk = (time_fun (fun a -> (^*) (fst a) (snd a)) (p1,p2)) in
    let tn = (time_fun (fun a -> mult_naive (fst a) (snd a)) (p1,p2)) in
    let tc = (time_fun (fun a -> toom_cook (fst a) (snd a)) (p1,p2)) in
    Some (((fst v), [|tk;tn;tc|]))
;;

(* initialise la  *)
let default_value_ht deg = ( (create_array_polynome 10 deg) , (Array.make 3 5.));;

let export_latex ht (file:string) = 
    let oc = open_out file in    (* create or truncate file, return channel *)
    Hashtbl.iter (fun k v -> Printf.fprintf oc "%d %f %f %f\n" k (snd v).(0) (snd v).(1) (snd v).(2) ) ht;
    close_out oc;
;;

let main =
    let a1 = make_array 0 10 in
    let a2 = make_array 11 2000 in (* TODO remplacer par 10000 *)
    shuffle a2;
    let a2 = Array.sub a2 0 ((1000 - (Array.length a1))) in
    let a1 = Array.append a1 a2;
    let ht = Hashtbl.create 1000 in
    Array.iter (fun a -> Hashtbl.add ht a (default_value_ht a)) a1;
    Hashtbl.filter_map_inplace run ht;
    print_float (snd (Hashtbl.find ht 3)).(1)
    (* print_string string_of_float (snd (Hashtbl.find ht 3)).(1) *)
;;