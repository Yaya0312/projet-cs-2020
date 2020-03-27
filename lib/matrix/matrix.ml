type 'a matrix = 'a array array;;

let get_el (m:'a matrix) (row:int) (col:int) : ('a) = 
  m.(row).(col)
;;

let get_row (m:'a matrix) (row:int) : ('a array) = 
  Array.get m row
;;

let get_col (m:'a matrix) (col:int) : ('a array) = 
  Array.map (fun m -> Array.get m col) m
;;

let mult_row (m:float array) (coef:float) : float array =
  Array.map (fun n -> n *. coef) m
;;

let op_row (f:float->float->float) (row1:float array) (row2:float array) : float array =
  Array.map2 f row1 row2
;;

let sum_row  = op_row (+.);;
let sub_row = op_row (-.);;

let swap_row (m:'a matrix) (row1:int) (row2:int) : unit =
  let tmp = m.(row1) in
  m.(row1) <- m.(row2);
  m.(row2) <- tmp
;;

let max_pos_array (b:float array) (from:int) : int = (* TODO from en option *)
  let rec aux (a:float array) (e_max:float) (p_max:int) (pos:int): int =
    if (pos = Array.length a) then
      p_max

    else if (e_max < Base.Float.abs(Array.get a pos)) then
      aux a (Array.get a pos) pos (pos + 1)
    else 
      aux a e_max p_max (pos + 1)
  in aux b b.(from) from from
;;



let solve (m:float matrix) : float array =
  let descente col_pos row_pos = 
    let max_pos_el = max_pos_array (get_col m col_pos) row_pos in
    if (max_pos_el <> row_pos) then 
      swap_row m max_pos_el row_pos;
    let f = (get_el m row_pos col_pos) /. (get_el m col_pos col_pos) in
    let a = mult_row (get_row m row_pos) f in
    let b = sub_row (get_row m row_pos) a in
    b
  in descente 0 0
;;