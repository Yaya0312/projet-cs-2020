(** le premier est le degree et le deuxieme le coefficient *)
type monome = (int*float);;
type poly = monome list;;

(* OPTIMISÉ AU MAX *)
let rec coef (p:poly) (i:int) : float = match p with
  | [] -> 0.
  | (d,_)::_ when d > i -> 0.
  | (d,c)::_ when i = d -> c
  | _::lst -> (coef [@tailcall]) lst i
;;

(* TODO CLEAN CODE raccourcir *)
let (^+) (p1:poly) (p2:poly) : poly = 
  let rec aux p1 p2 (r:poly) = match p1, p2 with
    | [],[] -> (List.rev r)
    | lst1, [] -> List.rev_append r lst1
    | [], lst2 -> List.rev_append r lst2
    | (d1,c1)::lst1, (d2,_)::_ when d1 < d2 -> 
        (aux [@tailcall]) lst1 p2 ((d1, c1)::r)
    | (d1,c1)::lst1, (d2,c2)::lst2 when d1 = d2 -> 
        (aux [@tailcall]) lst1 lst2 (
          begin 
            let p = c1 +. c2 in
            if (p = 0.) then r else ((d1, p)::r)
          end)
    | _, (d2,c2)::lst2 ->
        (aux [@tailcall]) p1 lst2 ((d2, c2)::r)
  in aux p1 p2 []
;;

(* OPTIMISÉ AU MAX *)
let multCoeff (p:poly) (a:float) : poly = match a with
  | 0. -> []
  | _ -> List.map (fun (d, c) ->(d, c*. a)) p
;;

let (^.) (a:float) (p:poly) = multCoeff p a;;

let (^-) (p1:poly) (p2:poly) : poly = 
  p1 ^+ ((-1.) ^. p2)
;;

let (^=) (p1:poly) (p2:poly) : bool = 
  (p1 ^- p2) = []
;;

(* OPTIMISATION MAX IMPOSSIBLE. La liste n'a pas de pointeur de queue*)
let degre (p:poly) : int = match p with
  | [] -> (-1)
  | _ -> List.rev p |> List.hd |> fst
;;

let multXn (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> (d + deg, c)) p
;;

let (^^) = multXn;;

(* OPTIMISÉ AU MAX *)
let cut (p:poly) (deg:int) = 
  match List.partition (fun (d, _) -> d < deg) p with
  | p0, p1 -> p0 ,( p1 ^^ (-deg))
;;

(* OPTIMISÉ AU MAX *)
let renverse (order:int) (p:poly) = 
  if (degre p) <= order then 
    List.rev_map (fun (d, c) -> (order - d, c)) p
  else 
    failwith "(deg p) > order"
;;

(* OPTIMISÉ AU MAX *)
let modulo (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> d - deg, c) p
;;

(* OPTIMISÉ AU MAX *)
let mono_to_string (m:monome) : string = match m with
  | (d,c) -> (if (c >= 0.) then ("+") else "")^
             (string_of_int (int_of_float c)) ^ 
             (if (d = 0) then "" else "x^" ^ (string_of_int d) ^ " ")
;;

(* OPTIMISÉ AU MAX *)
let print_poly (p:poly) : unit = 
  let rec aux (p:poly) (s:string) = match p with
    | [] -> (print_string (s^"\n"))
    | (d,c)::lst -> aux lst ((mono_to_string (d,c))^s)
  in aux p ""
[@@coverage off]
;;

(* OPTIMISÉ AU MAX *)
let horner (p:poly) (x:float) : float = 
  let rec aux (acc:float) (pos:int) (p:poly) = match p with 
    | [] -> acc
    | (d,c)::lst when d = pos -> c +. (x *. (aux acc (pos+1) lst))
    | _  -> (coef p pos) +. (x *. (aux acc (pos+1) p))
  in aux 0. 0 p
;;

let random_poly (deg:int) ?(maxcoef=1073741823) (): poly = 
  let rec aux (r:poly) (pos:int) = 
    if (pos = 0) then 
      r
    else 
      let coef = float_of_int (Random.int (maxcoef)) in
      let coef = if coef = 0. then 1. else coef in
      aux ((pos, coef)::r) (pos - 1)
  in aux [] deg
[@@coverage off]
;;

(* OPTIMISÉ AU MAX *)
let rec mult_naive (p1:poly) (p2:poly) : poly = match p1 with
  | [] -> []
  | (d,c)::[] -> List.map (fun (a,b) -> (a + d, b *. c) ) p2
  | e1::lst -> (mult_naive [e1] p2) ^+ (mult_naive lst p2)
;;

(** VERSION OPTI *)
let karatsuba = 
  let rec (^*) (p1:poly) (p2:poly) : poly =  match p1, p2 with
    |([], _) | (_, []) -> []
    | e1::[], e2::[] -> mult_naive [e1] [e2]
    | _ -> 
        let k = (max (degre p1) (degre p2)) in
        let k = k + (k mod 2) in
        let mk = (k / 2) in
        let p1_0,p1_1 = cut p1 mk
        and p2_0,p2_1 = cut p2 mk in
        let c0 = p1_0 ^* p2_0
        and c2 = p1_1 ^* p2_1
        and u = (p1_0 ^+ p1_1) ^* (p2_0 ^+ p2_1) in
        let c1 =  (u ^- c0) ^- c2 in
        c0 ^+ (c1 ^^ mk) ^+ (c2 ^^ k)
  in (^*)
;;


let rec toom_cookv2 (poly1:poly) (poly2:poly) (alpha:float) = match poly1, poly2 with
  |([], _) | (_, []) -> []
  |([(0, 0.)], _) | (_ ,[(0, 0.)]) -> [(0, 0.)]
  |([(0, b)], p)  | (p, [(0, b)]) -> (b ^. p)
  | _ ->
      let k = (max (degre poly1) (degre poly2)) in
      let k = k + 3 - (k mod 3) in
      let mk = (k / 3) in
      let p0, p_temp = cut poly1 mk
      and q0, q_temp = cut poly2 mk in
      let p1, p2 = cut p_temp mk
      and q1, q2 = cut q_temp mk in
      let n = degre p0 + 1 in
      let alpha_squared = alpha *. alpha in
      let var1 = -1. /. (2. *. (alpha -. 1.))
      and var2 = -1. /. (2. *. (alpha +. 1.))
      and var3 = 1. /. (alpha *. (alpha_squared -. 1.))
      and var6 = 1. /. alpha in
      let r0 = toom_cookv2 p0 q0 alpha in
      let r1 = toom_cookv2 (p0 ^+ p1 ^+ p2) (q0 ^+ q1 ^+ q2) alpha
      and r2 = toom_cookv2 ((p0 ^- p1) ^+ p2) ((q0 ^- q1) ^+ q2) alpha
      and r3 = toom_cookv2 
          (p0 ^+ (alpha ^. p1) ^+ (alpha_squared ^. p2))
          (q0 ^+ (alpha ^. q1) ^+ (alpha_squared ^. q2))
          alpha
      and r4 = toom_cookv2 p2 q2 alpha in
      let res1 = (-.var6 ^. r0) ^+
                 (-.alpha *. var1 ^. r1) ^+
                 (-.alpha *. var2 ^. r2) ^+
                 (-.var3 ^. r3) ^+
                 (alpha ^. r4) 
      and res2 = 0.5 ^. (r1 ^+ r2 ^- (2. ^. (r0 ^+ r4)))
      and res3 = (var6 ^. r0) ^+
                 (var1 ^. r1) ^+
                 (var2 ^. r2) ^+
                 (var3 ^. r3) ^+
                 (-.alpha ^. r4) in
      r0 ^+ (res1 ^^ n) ^+ (res2 ^^ (2 * n)) ^+ (res3 ^^ (3 * n)) ^+
      (r4 ^^ (4 * n))
;;

let toom_cook3 (p1:poly) (p2:poly) (alpha:float) =
  if (alpha = 1. || alpha = 0. || alpha = -1.) then
    failwith "Donner un autre alpha, DIVISION PAR 0"
  else
    toom_cookv2 p1 p2 alpha
;; 