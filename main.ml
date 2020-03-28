open BenchTools;;

let t1 = make_array 0 10000;;
shuffle t1 ~start:10 ();;
let t1 = Array.sub t1 0 1000;;
let ht = Hashtbl.create 1000;;
Array.iter (fun a -> Hashtbl.add ht a (calc a)) t1;;
export_latex ht "mult.dat";;