open BenchTools;;

let a1 = make_array 0 10;; (* Ok *)
let a2 = make_array 11 10000;; (* Ok *)
shuffle a2;; (* Ok *)
let a2 = Array.sub a2 0 ((1000 - (Array.length a1)));;
let a1 = Array.append a1 a2;;
let ht = Hashtbl.create 1000;;
Array.iter (fun a -> Hashtbl.add ht a (calc a)) a1;;
export_latex ht "mult.dat";;
;;