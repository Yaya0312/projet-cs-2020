type matrix = float array array;;
(* TODO demander prof pour rendre generique*)

let get_row (m:matrix) (row:int) : (float array) = 
    Array.get m row
;;

let get_col (m:matrix) (col:int) : (float array) = 
    Array.map (fun m -> Array.get m col) m
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

let max_pos_array (a:float array) : int = (* TODO *)
  let pos = ref (min_float, 0, -1) (* val_max, current_pos, max_pos *)
  Array.iter (fun e ->  ) a

;;

let solve (m:matrix) (row:int) : int = 
  
;;