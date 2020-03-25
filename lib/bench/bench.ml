exception MaxLowerThanMin;;
open Polynome;;
(*** TOOLS ********************************************************************)

(** 
   Interverti les valeurs a[i] a[j]
   Complexité O(1)
*)
let swap (a:'a array) (i:int) (j:int) : unit =
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t
;;

(** 
   Mélange le tableau
   Complexité O(n) n étant la longueur du tableau
*)
let shuffle (a:'a array) : unit =
  Array.iteri (fun i _ -> swap a i (Random.int (i + 1))) a
;;

(**
   Génére un tableau de min à max
   Complexité O(n) n étant la longueur du tableau
*)
let make_array (min:int) (max:int) : int array =
  let r = max - min in
  if r < 0 then raise MaxLowerThanMin else 
    Array.init (r + 1) (fun i -> (i + min))
;;

(** END TOOLS *)

let create_array_polynome (num:int) (deg:int) = 
  Array.init (num + 1) (fun _ -> random_poly deg max_float);;
;;

(** 
   Pioche deux element dans la liste
   Complexité O(1)
*)
let pick a =
  let random1 = Random.int (Array.length a) and
  random2 = Random.int (Array.length a) in
  a.(random1), a.(random2)
;;

(** Execute la fonction iter fois et retourne la moyenne *)
let average (iter:int) f : float =
  let rec aux acc pos = 
    if (pos = 0) then
      acc /. (float_of_int iter)
    else 
      aux (acc +. f) (pos + 1)
  in aux 0. iter
;;

(** 
   Retourne le temps que la fonction à mis pour s'executer
*)
let time_fun func arg =
  let start_time = Sys.time () in
  ignore (func arg);
  let finish_time = Sys.time () in
  finish_time -. start_time
;;


let export_latex ht (file:string) = 
  let oc = open_out file in  (* create or truncate file, return channel *)
  Printf.fprintf oc "# Génération : %f (unix time) \n" (Unix.time ());
  Hashtbl.iter (fun k v -> Printf.fprintf oc "%d %f \n" k v.(0)  ) ht;
  close_out oc;
;;

let calc (deg:int) : (float array) =
  let pList = create_array_polynome 10 deg in
  let p1,p2 = pick pList in 
  (* let tk = time_fun (fun _ -> karatsuba  p1 p2) () in
     let tn = time_fun (fun _ -> mult_naive p1 p2) () in *)
  let tc = time_fun (fun _ -> Polynome.toom_cook p1 p2) () in
  [|tc|]
;;

let a1 = make_array 0 10;; (* Ok *)
let a2 = make_array 11 10000;; (* Ok *)
shuffle a2;; (* Ok *)
let a2 = Array.sub a2 0 ((1000 - (Array.length a1)));;
let a1 = Array.append a1 a2;;
let a2 = [];;
let ht = Hashtbl.create 1000;;
Array.iter (fun a -> Hashtbl.add ht a (calc a)) a1;;
export_latex ht "mult.dat";;
;;