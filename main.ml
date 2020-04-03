open BenchTools;;

let sample = 200;;
let maxDeg = 10_000;;
let t1 = make_array 0 maxDeg;;
shuffle t1 ~start:10 ();;
let t1 = Array.sub t1 0 sample;;
let ht = Hashtbl.create ~random:false sample;;
Array.iter (fun a -> Hashtbl.add ht a (calc a)) t1;;
export_latex ht "mult.dat";;