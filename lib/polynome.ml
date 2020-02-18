(** le premier est le degree et le deuxieme le coefficient *)
type monome = (int*float);;
type poly = monome list;;

(* Complexity O(n) n étant la longueur de la liste p*)
let rec coef (p:poly) (i:int) = match p with
  | [] -> 0.
  | (d,_)::_ when d > i -> 0.
  | (d,c)::_ when i = d -> c
  | _::lst -> coef lst i
;;

(* Complexity O(n) n étant la longueur de la liste p1 ou p2 *)
let sum (p1:poly) (p2:poly) = 
  let rec aux p1 p2 (r:poly) = match p1,p2 with
    | [],[] -> (List.rev r)
    | lst1, [] -> List.rev_append r lst1
    | [], lst2 -> List.rev_append r lst2
    | (d1,c1)::lst1, (d2,_)::_ when d1 < d2 -> aux lst1 p2 ((d1,c1)::r)
    | (d1,c1)::lst1, (d2,c2)::lst2 when d1 = d2 -> aux lst1 lst2
      (let p = c1+.c2 in if (p = 0.) then r else ((d1,p)::r))
    | _, (d2,c2)::lst2 -> aux p1 lst2 ((d2,c2)::r)
  in aux p1 p2 []
;;

let (^+) = sum;;

(* Complexity O(n) n étant la longueur de la liste p*)
let multCoeff (p:poly) (a:float) = match a with
  | 0. -> []
  | _ -> List.map (fun (d,c) ->(d, c*.a)) p
;;

let (^:) = multCoeff;;

(* Complexity O(n) n étant la longueur de la liste p1*)
let (^-) (p1:poly) (p2:poly) = 
  p1 ^+ (p2 ^: (-1.))
;;

(* Complexity O(1) n étant la longueur de la liste p1*)
let (^=) (p1:poly) (p2:poly) = 
  (p1 ^- p2) = []
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let degre (p:poly) = match p with
  | [] -> (-1)
  | _ -> List.rev p |> List.hd |> fst
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let multXn (p:poly) (deg:int) =
  List.map (fun (d,c) -> (d+deg,c)) p
;;

let (^^) = multXn;;

(* Complexity O(n) n étant la longueur de la liste p*)
let cut (p:poly) (deg:int) = 
  match List.partition (fun (d,_) -> d < deg) p with
  | p0,p1 -> p0 ,( p1 ^^ (-deg))
;;

let rec (^*) (p1:poly) (p2:poly) =  match p1, p2 with
  |([], _) | (_, []) -> []
  |([(0, 0.)], _) | (_ ,[(0, 0.)]) -> [(0, 0.)]
  |([(0, b)], p)  |(p, [(0, b)]) -> (p ^: b)
  | _ -> 
  let k = (max (degre p1) (degre p2)) in
  let k = k + (k mod 2) in
  let mk = (k/2) in
  let p1_0,p1_1 = cut p1 mk in
  let p2_0,p2_1 = cut p2 mk in
  let c0 = p1_0 ^* p2_0 in
  let c2 = p1_1 ^* p2_1 in
  let u = (p1_0 ^+ p1_1) ^* (p2_0 ^+ p2_1) in
  let c1 =  (u ^- c0) ^- c2 in
  c0 ^+ (c1 ^^ mk) ^+ (c2 ^^ k)
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let renverse (order:int) (p:poly) = 
  if (degre p) <= order then 
    List.rev_map (fun (d,c) -> (order-d,c)) p
  else 
    failwith "(deg p) > order"
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let modulo (p:poly) (deg:int) =
  List.map (fun (d,c) -> (d-deg,c)) p
;;

(* let (^@) = multCoeff;; *)

(* Todo inversion *)
(* let inversion (p:poly) (pos_max:int) =
  let rec aux (g_i:int) (pos:int) (p1:poly) = 
    let g_i = ((p^@2.)^-(p^*g_i^*g_i)) in
    if (pos < pos_max) then 
      (aux (g_i+1) (pos+1) (modulo ))
    else 
      p1
  in aux 1 0
;; *)

(* Complexity O(1) *)
let mono_to_string (m:monome) = match m with
  | (d,c) -> (if (c >= 0.) then ("+") else "")^
  (string_of_int (int_of_float c)) ^ 
  (if (d = 0) then "" else "x^" ^ (string_of_int d) ^ " ")
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let print_poly (p:poly) = 
  let rec aux (p:poly) (s:string) = match p with
  | [] -> (print_string (s^"\n"))
  | (d,c)::lst -> aux lst ((mono_to_string (d,c))^s)
  in aux p ""
;;

(* Complexity O(n) n étant la longueur de la liste p*)
let horner (p:poly) (x:float) = 
    let rec aux (acc:float) (pos:int) (p:poly) = match p with 
    | [] -> acc
    | (d,c)::lst when d=pos -> c +. (x *. (aux acc (pos+1) lst))
    | _  -> (coef p pos) +. (x *. (aux acc (pos+1) p))
    in aux 0. 0 p
;;

(* Complexity O(n) n étant le degré maximal possible du polynome *)
let random_poly (deg:int) (maxcoef:int) = 
  let rec aux (r:poly) (pos:int) = match pos with
  | _ when pos = deg -> (List.rev r)
  | _ -> 
  let has_coef = Random.bool () in
  let coef = float_of_int (Random.int (maxcoef)) in
  if (has_coef) then 
    aux ((pos, coef)::r) (pos +1)
  else 
    aux r (pos+1)
  in aux [] 0
;;