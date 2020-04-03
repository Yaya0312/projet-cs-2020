(** le premier est le degree et le deuxieme le coefficient *)
type monome = (int*float);;
type poly = monome list;;

let rec coef (p:poly) (i:int) : float = match p with
  | [] -> 0.
  | (d,_)::_ when d > i -> 0.
  | (d,c)::_ when i = d -> c
  | _::lst -> (coef [@tailcall]) lst i
;;

let (^+) (p1:poly) (p2:poly) : poly = 
  let rec aux p1 p2 (r:poly) = match p1,p2 with
    | [],[] -> (List.rev r)
    | lst1, [] -> List.rev_append r lst1
    | [], lst2 -> List.rev_append r lst2
    | (d1,c1)::lst1, (d2,_)::_ when d1 < d2 -> aux lst1 p2 ((d1,c1)::r)
    | (d1,c1)::lst1, (d2,c2)::lst2 when d1 = d2 -> 
        (aux [@tailcall]) lst1 lst2 (
          begin 
            let p = c1+.c2 in
            if (p = 0.) then r else ((d1,p)::r)
          end)
    | _, (d2,c2)::lst2 -> (aux [@tailcall]) p1 lst2 ((d2,c2)::r)
  in aux p1 p2 []
;;

let multCoeff (p:poly) (a:float) : poly = match a with
  | 0. -> []
  | _ -> List.map (fun (d,c) ->(d, c*.a)) p
;;

let (^:) = multCoeff;;

let (^-) (p1:poly) (p2:poly) : poly = 
  p1 ^+ (p2 ^: (-1.))
;;

let (^=) (p1:poly) (p2:poly) : bool = 
  (p1 ^- p2) = []
;;

let degre (p:poly) : int = match p with
  | [] -> (-1)
  | _ -> List.rev p |> List.hd |> fst
;;

let multXn (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> (d+deg,c)) p
;;

let (^^) = multXn;;

let cut (p:poly) (deg:int) = 
  match List.partition (fun (d,_) -> d < deg) p with
  | p0,p1 -> p0 ,( p1 ^^ (-deg))
;;

let renverse (order:int) (p:poly) = 
  if (degre p) <= order then 
    List.rev_map (fun (d,c) -> (order-d,c)) p
  else 
    failwith "(deg p) > order"
;;

let modulo (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> (d-deg,c)) p
;;

let mono_to_string (m:monome) : string = match m with
  | (d,c) -> (if (c >= 0.) then ("+") else "")^
             (string_of_int (int_of_float c)) ^ 
             (if (d = 0) then "" else "x^" ^ (string_of_int d) ^ " ")
;;

let print_poly (p:poly) : unit = 
  let rec aux (p:poly) (s:string) = match p with
    | [] -> (print_string (s^"\n"))
    | (d,c)::lst -> aux lst ((mono_to_string (d,c))^s)
  in aux p ""
;;

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
;;

let rec mult_naive (p1:poly) (p2:poly) : poly = match p1 with
  | [] -> []
  | (d,c)::[] -> List.map (fun (a,b) -> (a+d,b*.c) ) p2
  | e1::lst -> (mult_naive [e1] p2) ^+ (mult_naive lst p2)
;;

let karatsuba = 
  let rec (^*) (p1:poly) (p2:poly) : poly =  match p1, p2 with
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
  in (^*) 
;;

let rec toom_cook (p1:poly) (p2:poly) (alpha:float) = match p1, p2 with
  |([], _) | (_, []) -> []
  |([(0, 0.)], _) | (_ ,[(0, 0.)]) -> [(0, 0.)]
  |([(0, b)], p)  | (p, [(0, b)]) -> (p ^: b)
  | _ ->
      let k = (max (degre p1) (degre p2)) in
      let k = k + 3 - (k mod 3) in
      let mk = (k/3) in
      let p1_0, p_temp = cut p1 mk in
      let p1_1, p1_2 = cut p_temp mk in
      let p2_0, q_temp = cut p2 mk in
      let p2_1, p2_2 = cut q_temp mk in
      let n = degre p1_0 + 1 in
      let r0 = toom_cook p1_0 p2_0 alpha in 
      let r1 = (toom_cook (p1_0 ^+ p1_1 ^+ p1_2) (p2_0 ^+ p2_1 ^+ p2_2) alpha) in
      let r2 = (toom_cook (p1_0 ^+ p1_2 ^+ (p1_1 ^: (-1.))) (p2_0 ^+ p2_2 ^+ (p2_1 ^: (-1.))) alpha) in
      let r3 = (toom_cook (p1_0 ^+ (p1_1 ^: alpha) ^+ (p1_2 ^: (alpha*.alpha))) (p2_0 ^+ (p2_1 ^: alpha) ^+ (p2_2 ^: (alpha*.alpha))) alpha) in
      let r4 = (toom_cook p1_2 p2_2 alpha) in
      let res0 = r0 in
      let res1 = ((r0 ^: (-1.)) ^: (1./.alpha)) ^+ (r1 ^: (alpha /. (2. *. (alpha -. 1.)))) ^+ (r4 ^: alpha) ^+ (r2 ^: ((alpha *. -1.) /. (2. *. (alpha +. 1.)))) ^+ (r3 ^:  (-1. /. (alpha *. ((alpha *. alpha) -. 1.)))) in
      let res2 = (r0 ^: (-1.)) ^+ (r4 ^: (-1.)) ^+ ((r1 ^+ r2) ^: (1./.2.)) in
      let res3 = (r0 ^: (1./.alpha)) ^+ (r1 ^: (-1. /. (2. *. (alpha -. 1.)))) ^+ (r4 ^: (alpha *. -1.)) ^+ (r2 ^: (-1. /. (2. *. (alpha +. 1.))))  ^+ (r3 ^: (1. /. (alpha *. ((alpha *. alpha) -. 1.)))) in
      let res4 = r4 in
      res0 ^+ (res1 ^^ n) ^+ (res2 ^^ (2*n)) ^+ (res3 ^^ (3*n)) ^+ (res4 ^^ (4*n))
;;

let toom_cook3 (p1:poly) (p2:poly) (alpha:float) =
  if (alpha = 1. || alpha = 0. || alpha = -1.) then
    failwith "Donner un autre alpha, DIVISION PAR 0"
  else
    toom_cook p1 p2 alpha
;; 