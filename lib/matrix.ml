type matrix = float array array;;
(* TODO demander prof pour rendre generique*)

let get_row (m:matrix) (row:int) : (float array) = 
  Array.get m row
;;

let get_col (m:matrix) (col:int) : (float array) = 
  Array.map (fun m -> Array.get m col) m
;;

let is_squared (m:matrix) : bool =
  (Array.length m) = (Array.length m.(0))
;;

let get_diag (m:matrix) : (float array) =
  let pos = ref (-1) in
  Array.map (fun m -> incr pos; Array.get m !pos) m
;;

let mul_row (m:matrix) (row:int) (coef:float) : unit =
  Array.set m row (Array.map (fun n -> n *. coef) m.(row))
;;

let sum_row (m:matrix) (num_row:int) (row:'a array) : unit =
  Array.set m row (Array.map (fun n -> n *. coef) m.(row))
;;

let swap_row (m:matrix) (row1:int) (row2:int) : unit =
  let tmp = m.(row1) in
  m.(row1) <- m.(row2);
  m.(row2) <- tmp
;;

let max_pos_array (a:float array) = (* TODO *)
  let max_pos = ref 0;
    let current_pos = ref (-1);
      let f (e:float) : unit = 
        incr current_pos;
        (if ((Float.abs e) > (Array.get a !current_pos)) then 
           max_pos := !current_pos);
        ()
        ;
        Array.iter f a
;;

let max_v2 (b:float array) : int = 
  let rec aux (a:float array) (e_max:float) (p_max:int) (pos:int): int =
    if (pos = Array.length a) then
      p_max
    else if (e_max < Float.abs(Array.get a pos)) then
      aux a (Array.get a pos) pos (pos+1)
    else 
      aux a e_max p_max (pos+1)
  in aux b (Array.get b 0) 0 0
;;

let solve (m:matrix) (row:float array) : float array = 

;;