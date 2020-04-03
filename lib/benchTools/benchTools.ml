exception MaxLowerThanMin;;
open Polynome;;
(*** TOOLS ********************************************************************)

let swap (a:'a array) (i:int) (j:int) : unit =
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t
;;

let shuffle (a:'a array) ?(start = 0) ?(len = (Array.length a)-start) () 
  : unit =
  Array.iteri (fun i _ -> swap a i (Random.int (i + 1))) (Array.sub a start len)
;;

let make_array (min:int) (max:int) : int array =
  let r = max - min in
  if r < 0 then raise MaxLowerThanMin else 
    Array.init (r + 1) (fun i -> (i + min))
;;

(** 
   [time_fun func arg] retourne le temps que la fonction fun à mis pour 
   s'executer
*)
let time_fun func arg=
  let start_time = Sys.time () in
  ignore(func arg);
  Sys.time () -. start_time
;;

(*** END TOOLS ****************************************************************)

(** 
   [pick_double a] pioche aléatoirement deux elements dans la liste a \n
   Complexité O(1)
*)
let pick_double a =
  let pick l = a.(Random.int (Array.length l)) in
  (pick a),(pick a)
;;

let export_latex ht (file:string) : unit = 
  let oc = open_out file in
  Printf.fprintf oc "# Génération temps multiplication \n";
  Printf.fprintf oc "P $Naive$ $Kara$ $Toom3$ \n";
  Hashtbl.iter (fun k v -> Printf.fprintf oc 
                   "%d %f %f %f \n" 
                   k v.(0) v.(1) v.(2))
    ht;
  close_out oc;
;;

let calc (deg:int) : (float array) =
  let p1,p2 = (Array.init 11 (fun _ -> random_poly deg ()) ) |> pick_double in 
  [|
    time_fun (fun _ -> mult_naive p1 p2) ();
    time_fun (fun _ -> karatsuba  p1 p2) ();
    time_fun (fun _ -> toom_cook3 p1 p2) ();
  |]
;;
