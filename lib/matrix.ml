type 'a matrix = 'a array array;;

let get_row (m:'a matrix) (row:int) : ('a array) = 
  Array.get m row
;;

let get_col (m:'a matrix) (col:int) : ('a array) = 
  Array.map (fun m -> Array.get m col) m
;;

let get_diag (m:'a matrix) : ('a array) =
  let pos = ref (-1) in
  Array.map (fun m -> incr pos; Array.get m !pos) m
;;

let mul_row (m:float matrix) (row:int) (coef:float) : unit =
  Array.set m row (Array.map (fun n -> n *. coef) m.(row))
;;

let sum_row (m:float matrix) (row_num:int) (row:float array) : unit =
  let c = Array.map2 (+.) m.(row_num) row in
  Array.set m row_num c;
;;
let sub_row (m:float matrix) (row_num:int) (row:float array) : unit =
  let c = Array.map2 (-.) m.(row_num) row in
  Array.set m row_num c;
;;
let swap_row (m:'a matrix) (row1:int) (row2:int) : unit =
  let tmp = m.(row1) in
  m.(row1) <- m.(row2);
  m.(row2) <- tmp
;;

let max_pos_array (b:float array) : int = 
  let rec aux (a:float array) (e_max:float) (p_max:int) (pos:int): int =
    if (pos = Array.length a) then
      p_max
    else if (e_max < Float.abs(Array.get a pos)) then
      aux a (Array.get a pos) pos (pos + 1)
    else 
      aux a e_max p_max (pos + 1)
  in aux b (Array.get b 0) 0 0
;;

let add_row (m:float matrix) (num_diag:int) (num_row:int) : unit =
  let f = m.(num_row).(num_diag) /. m.(num_diag).(num_diag) in
  for j = num_diag + 1 to ((Array.length m) - 1) do
    (mul_row m j f);
    (sub_row m num_row m.(j));
  done;
  m.(num_row).(num_diag) <- 0.
;;

let max_col_from_diag (m:float matrix) (num_diag:int) : int =
  let rec max_col_from_diag_aux m num_diag i_max line =
    if (line < (Array.length m)) then
      if ((m.(line).(num_diag)) > (m.(i_max).(num_diag))) then
        (max_col_from_diag_aux m num_diag line (line + 1))
      else 
        (max_col_from_diag_aux m num_diag i_max (line + 1))
    else
      i_max
  in max_col_from_diag_aux m num_diag num_diag (num_diag + 1)
;;

let rec resolve_inf (m:float matrix) = 
  for i = 0 to (Array.length m) - 1 do 
    let i_max = max_col_from_diag m i in 
    swap_row m i i_max;
    add_row m i i_max;
  done;
;;