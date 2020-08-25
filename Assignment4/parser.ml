type token =
  | ADD
  | SUBT
  | MULT
  | DIV
  | SUM
  | ROWSUM
  | COLSUM
  | AVG
  | ROWAVG
  | COLAVG
  | MIN
  | ROWMIN
  | COLMIN
  | MAX
  | ROWMAX
  | COLMAX
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | COMMA
  | COLON
  | SEMICOLON
  | ASS
  | INT of (int)
  | FLOAT of (float)
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    type cell = None | Some of float;;
    type matrix2 = cell list list;;
    type matrix =float list list;;
    

    exception EmptyCell
    exception IncompatibleRanges
    exception IndexOutofRange


    let rec print_list lst =
    match lst with
    | [] -> " "
    | Some h::t ->print_float h ; print_string" "; print_list t
    | None::t ->print_string "None"; print_string" "; print_list t 
  ;;
   let rec print_mat mat =
   match mat with
   | [] -> " "
   | h::t -> print_list h ; print_string"\n" ; print_mat t
 ;;

 
    
let const_l s : cell =
  match s with 
  |"" -> None
  |" " -> None
  |a -> Some(float_of_string(a))
;;

  let rec const_m s : cell list =
  match s with
  |  [] ->[]
 | x::xs-> (const_l x):: (const_m xs)
;;

    let rec matrix_const (a:int) b : matrix2 =
  if(a=0) then [] else
  let line = input_line b in
    let split = String.split_on_char ',' in 
    let values = split line in
    (const_m values):: matrix_const (a-1) b
;;

let m=int_of_string(Sys.argv.(2));;
let n=int_of_string(Sys.argv.(3));;
let sheet3 : matrix2 =
  try let in_stream = open_in Sys.argv.(1) in
 let yo = (matrix_const m in_stream) in 
  close_in in_stream;
  yo;
  with e -> raise e
  ;;
  
    let sheet2 = sheet3;;
     
    (*let sheet2: matrix2 = [[Some 2.0;Some 5.0;Some 3.4;Some 5.2;None;None];[Some 2.0;Some 5.4;Some 7.0;Some 6.3;None;None];[Some 3.0;None;Some 1.0;None;None;None];[Some 2.5;None;None;None;None;None]];;*)
    let sheet: matrix=[[]];;
    let max_float (a:float) (b:float): float=
    if(a>=b) then a 
    else b
    ;;
    let min_float (a:float) (b:float): float=
    if(a>=b) then b 
    else a
    ;;
    
    let rec assign_l (lst:cell list) (count:float) (column:int): cell list=
      match lst,column with
      | x::xs ,0-> Some count::xs
      | x::xs ,_-> x::assign_l xs count (column-1)
      | [],_->[]
    ;;
    let rec assign_m (m:matrix2) (count:float) (row:int) (column:int):matrix2=
    if(row>0) then 
      match m with
        |x::xs-> x::(assign_m xs count (row-1) column)
        |[]->[]
    else 
      match m with 
        | x::xs->(assign_l x count column)::xs
        | []->[]      
    ;;
    let rec lst_dim (lst:float list): int =
    match lst with
    | x::xs -> 1+lst_dim xs
    | []->0
    ;;
    
    let rec calc_avg (a:float) (b:float): float=a/.b
    
    let rec transm (m:matrix2): matrix2 = 
    match m with
    [] -> []
    | [] :: c -> transm c
    | (h::t) :: c -> (h :: List.map (List.hd)  c) :: transm (t:: List.map (List.tl) c)
;;
    let rec assign_l_row (lst:cell list) (value:float) (column:int): cell list=
      if(column>0) then 
      match lst with
      | Some x::xs-> Some x::assign_l_row xs value (column-1)
      | None::xs->None::assign_l_row xs value (column-1)
      | []->[]
      else 
      match lst with
      | Some x::xs-> Some value::xs
      | None::xs->Some value::xs
      | []->[]

    ;;
    let rec assign_m_row  (m:matrix2) (lst:float list) (row:int) (column:int):matrix2=
    if(row>0) then 
      match m with
        |x::xs-> x::(assign_m_row xs lst (row-1) column)
        |[]->[]
    else 
      match m,lst with 
        | x::xs,h::t->(assign_l_row x h column)::(assign_m_row xs t row column)
        | x::xs,[]->x::xs 
        | [],h::t->raise IndexOutofRange
        | [],[]->[]     
    ;;
    let rec row_count_helper_l2 (lst:cell list) (b:int) (d:int):float =
    if (b>0) then match lst with
    | x::xs-> row_count_helper_l2 xs (b-1) (d-1)
    | []-> 0.0
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> 1.0 +. (row_count_helper_l2 xs (b) (d-1))
    | None::xs -> 0.0 +. (row_count_helper_l2 xs (b) (d-1))
    | []-> 0.0
    else 0.0
    ;;
    let rec row_count_helper_2 (m:matrix2) (a,b) (c,d):float list =
    
    if(a>0) then  
      match m with
        | x::xs->  (row_count_helper_2 xs (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (row_count_helper_l2 x b d)::(row_count_helper_2 xs (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
  let rec row_sum_helper_l2 (lst:cell list) (b:int) (d:int):float =
    if (b>0) then match lst with
    | x::xs-> row_sum_helper_l2 xs (b-1) (d-1)
    | []-> 0.0
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> x +. (row_sum_helper_l2 xs (b) (d-1))
    | None::xs -> raise EmptyCell
    | []-> 0.0
    else 0.0
    ;;
    let rec row_sum_helper_2 (m:matrix2) (a,b) (c,d):float list =
    
    if(a>0) then  
      match m with
        | x::xs->  (row_sum_helper_2 xs (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (row_sum_helper_l2 x b d)::(row_sum_helper_2 xs (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let rec calc_avg_row (list1:float list) (list2:float list):float list=
    match list1,list2 with
    |h1::t1,h2::t2-> h1/.h2::(calc_avg_row t1 t2)
    |[],[]->[]
    ;;
    let rec row_max_helper_l2 (lst:cell list) (b:int) (d:int):float =
    if (b>0) then match lst with
    | x::xs-> row_max_helper_l2 xs (b-1) (d-1)
    | []-> (-1000000000.)
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> max_float x (row_max_helper_l2 xs (b) (d-1))
    | None::xs -> raise EmptyCell
    | []-> (-1000000000.)
    else (-1000000000.)
    ;;
    let rec row_max_helper_2 (m:matrix2) (a,b) (c,d):float list =
    
    if(a>0) then  
      match m with
        | x::xs->  (row_max_helper_2 xs (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (row_max_helper_l2 x b d)::(row_max_helper_2 xs (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let rec row_min_helper_l2 (lst:cell list) (b:int) (d:int):float =
    if (b>0) then match lst with
    | x::xs-> row_min_helper_l2 xs (b-1) (d-1)
    | []-> (1000000000.)
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> min_float x (row_min_helper_l2 xs (b) (d-1))
    | None::xs -> raise EmptyCell
    | []-> (1000000000.)
    else (1000000000.)
    ;;
    let rec row_min_helper_2 (m:matrix2) (a,b) (c,d):float list =
    
    if(a>0) then  
      match m with
        | x::xs->  (row_min_helper_2 xs (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (row_min_helper_l2 x b d)::(row_min_helper_2 xs (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let col_count_helper_2 (m:matrix2) (a,b) (c,d) r s= transm (assign_m_row (transm m) (row_count_helper_2 (transm m) (b,a) (d,c)) s r);;
    let col_sum_helper_2 (m:matrix2) (a,b) (c,d) r s= transm (assign_m_row (transm m) (row_sum_helper_2 (transm m) (b,a) (d,c)) s r);;
    let col_avg_helper_2 (m:matrix2) (a,b) (c,d) r s= transm (assign_m_row (transm m) (calc_avg_row (row_sum_helper_2 (transm m) (b,a) (d,c)) (row_count_helper_2 (transm m) (b,a) (d,c))) s r);;
    let col_max_helper_2 (m:matrix2) (a,b) (c,d) r s= transm (assign_m_row (transm m) (row_max_helper_2 (transm m) (b,a) (d,c)) s r);;
    let col_min_helper_2 (m:matrix2) (a,b) (c,d) r s= transm (assign_m_row (transm m) (row_min_helper_2 (transm m) (b,a) (d,c)) s r);;
    
   let rec full_count_helper_2 (m:matrix2) (lst:float list): float=
      match lst with
      | x::xs-> x +. (full_count_helper_2 m xs)
      | []->0.0
    ;;
  let rec full_sum_helper_2 (m:matrix2) (lst:float list): float=
      match lst with
      | x::xs-> x +. (full_sum_helper_2 m xs)
      | []->0.0
    ;;
    let rec full_max_helper_2 (m:matrix2) (lst:float list): float=
      match lst with
      | x::xs-> max_float x (full_max_helper_2 m xs)
      | []->(-1000000000.)
    ;;
    let rec full_min_helper_2 (m:matrix2) (lst:float list): float=
      match lst with
      | x::xs->min_float x (full_min_helper_2 m xs)
      | []->(1000000000.)
    ;;
    let rec write_l (lst:cell list) (value:float list) (column:int): cell list=
      if(column>0) then 
      match lst with
      | Some x::xs-> Some x::write_l xs value (column-1)
      | None::xs->None::write_l xs value (column-1)
      | []->[]
      else 
      match lst,value with
      | Some x::xs,h::t-> Some h::write_l xs t column
      | None::xs,h::t->Some h::write_l xs t column
      | Some x::xs,[]-> Some x::xs
      | None::xs,[]->None::xs
      | [],x::xs->raise IndexOutofRange
      | [],[]->[]

    ;;
    let rec write_m  (m:matrix2) (m2:float list list) (row:int) (column:int):matrix2=
    if(row>0) then 
      match m with
        |x::xs-> x::(write_m xs m2 (row-1) column)
        |[]->[]
    else 
      match m,m2 with 
        | x::xs,h::t->(write_l x h column)::(write_m xs t row column)
        | x::xs,[]->x::xs 
        |[],x::xs->raise IndexOutofRange
        | [],[]->[]     
    ;;
   let rec add_const_helper_l2 (lst:cell list) (f:float) (b:int) (d:int):float list=
    if (b>0) then match lst with
    | x::xs-> add_const_helper_l2 xs f (b-1) (d-1)
    | []-> []
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> (x +. f)::(add_const_helper_l2 xs f b (d-1))
    | None::xs -> raise EmptyCell
    | []-> []
    else []
    ;;
    let rec add_const_helper_2 (m:matrix2) (f:float) (a,b) (c,d):float list list =
    
    if(a>0) then  
      match m with
        | x::xs->  (add_const_helper_2 xs f (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (add_const_helper_l2 x f b d)::(add_const_helper_2 xs f (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;; 
    let rec sub_const_helper_l2 (lst:cell list) (f:float) (b:int) (d:int):float list=
    if (b>0) then match lst with
    | x::xs-> sub_const_helper_l2 xs f (b-1) (d-1)
    | []-> []
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> (x -. f)::(sub_const_helper_l2 xs f b (d-1))
    | None::xs -> raise EmptyCell
    | []-> []
    else []
    ;;
    let rec sub_const_helper_2 (m:matrix2) (f:float) (a,b) (c,d):float list list =
    
    if(a>0) then  
      match m with
        | x::xs->  (sub_const_helper_2 xs f (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (sub_const_helper_l2 x f b d)::(sub_const_helper_2 xs f (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let rec mult_const_helper_l2 (lst:cell list) (f:float) (b:int) (d:int):float list=
    if (b>0) then match lst with
    | x::xs-> mult_const_helper_l2 xs f (b-1) (d-1)
    | []-> []
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> (x *. f)::(mult_const_helper_l2 xs f b (d-1))
    | None::xs -> raise EmptyCell
    | []-> []
    else []
    ;;
    let rec mult_const_helper_2 (m:matrix2) (f:float) (a,b) (c,d):float list list =
    
    if(a>0) then  
      match m with
        | x::xs->  (mult_const_helper_2 xs f (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (mult_const_helper_l2 x f b d)::(mult_const_helper_2 xs f (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let rec div_const_helper_l2 (lst:cell list) (f:float) (b:int) (d:int):float list=
    if (b>0) then match lst with
    | x::xs-> div_const_helper_l2 xs f (b-1) (d-1)
    | []-> []
    else 
    if(d>=0) then  
     match lst with
    | Some x::xs-> (x /. f)::(div_const_helper_l2 xs f b (d-1))
    | None::xs -> raise EmptyCell
    | []-> []
    else []
    ;;
    let rec div_const_helper_2 (m:matrix2) (f:float) (a,b) (c,d):float list list =
    
    if(a>0) then  
      match m with
        | x::xs->  (div_const_helper_2 xs f (a-1,b) (c-1,d))
        | []->[]
    else
      if (c>=0) then  
        match m with
          | x::xs -> (div_const_helper_l2 x f b d)::(div_const_helper_2 xs f (a,b) (c-1,d))
          | []->[]
      else 
        match m with
          | x::xs -> []
          | []->[]
    ;;
    let rec value_l (lst:cell list) (b:int): float=
    if(b>0)then match lst with
    | Some x::xs-> value_l xs (b-1)
    | None::xs-> value_l xs (b-1)
    else match lst with
    | Some x::xs-> x
    | None::xs->raise EmptyCell
    | []->raise EmptyCell
    ;;
    let rec value(m:matrix2) (a,b):float = 
      if(a>0) then match m with
      |x::xs->value xs (a-1,b)
      else match m with
      |x::xs->value_l x b 

    ;;
    let rec add_range_helper_l2 (list1:cell list) (list2:cell list) (b1:int) (d1:int) (b2:int) (d2:int):float list=
    if (b1>0) then match list1 with
    | x::xs-> add_range_helper_l2 xs list2 (b1-1) (d1-1) b2 d2
    | []-> []
    else 
    if (b2>0) then match list2 with
    | x::xs-> add_range_helper_l2 list1 xs (b1) (d1) (b2-1) (d2-1)
    | []-> []

    else
    if(d1!=d2) then raise IncompatibleRanges
    else 
    if(d1>=0 && d2>=0) then  
     match list1,list2 with
    | Some h1::t1,Some h2::t2-> (h1 +. h2)::(add_range_helper_l2 t1 t2 b1 (d1-1) b2 (d2-1))
    | Some h1::t1,None::t2->raise EmptyCell
    | None::t1,Some h2::t2->raise EmptyCell
    | None::t1,None::t2 -> raise EmptyCell
    | [],[]-> []
    else []
    ;;
    

    let rec add_range_helper_2 (m1:matrix2) (m2:matrix2) (a1,b1) (c1,d1) (a2,b2) (c2,d2):float list list =
    
    if(a1>0) then  
      match m1 with
        | x::xs->  (add_range_helper_2 xs m2 (a1-1,b1) (c1-1,d1) (a2,b2) (c2,d2))
        | []->[]
      else
        if (a2>0) then  
          match m2 with
            | x::xs -> (add_range_helper_2 m1 xs (a1,b1) (c1,d1) (a2-1,b2) (c2-1,d2))
            | []->[]
        else 
         if(c1!=c2) then raise IncompatibleRanges
         else
         if(c1>=0 && c2>=0) then
           match m1,m2 with
            |h1::t1,h2::t2-> (add_range_helper_l2 h1 h2 b1 d1 b2 d2)::(add_range_helper_2 t1 t2 (a1,b1) (c1-1,d1) (a2,b2) (c2-1,d2))
            |h1::t1,[]->raise IndexOutofRange
            |[],h2::t2->raise IndexOutofRange
            |[],[]->[]
          else []
         
      
    ;;
    let rec sub_range_helper_l2 (list1:cell list) (list2:cell list) (b1:int) (d1:int) (b2:int) (d2:int):float list=
    if (b1>0) then match list1 with
    | x::xs-> sub_range_helper_l2 xs list2 (b1-1) (d1-1) b2 d2
    | []-> []
    else 
    if (b2>0) then match list2 with
    | x::xs-> sub_range_helper_l2 list1 xs (b1) (d1) (b2-1) (d2-1)
    | []-> []

    else
    if(d1!=d2) then raise IncompatibleRanges
    else 
    if(d1>=0 && d2>=0) then  
     match list1,list2 with
    | Some h1::t1,Some h2::t2-> (h1 -. h2)::(sub_range_helper_l2 t1 t2 b1 (d1-1) b2 (d2-1))
    | Some h1::t1,None::t2->raise EmptyCell
    | None::t1,Some h2::t2->raise EmptyCell
    | None::t1,None::t2 -> raise EmptyCell
    | [],[]-> []
    else []
    ;;
    

    let rec sub_range_helper_2 (m1:matrix2) (m2:matrix2) (a1,b1) (c1,d1) (a2,b2) (c2,d2):float list list =
    
    if(a1>0) then  
      match m1 with
        | x::xs->  (sub_range_helper_2 xs m2 (a1-1,b1) (c1-1,d1) (a2,b2) (c2,d2))
        | []->[]
      else
        if (a2>0) then  
          match m2 with
            | x::xs -> (sub_range_helper_2 m1 xs (a1,b1) (c1,d1) (a2-1,b2) (c2-1,d2))
            | []->[]
        else 
        if(c1!=c2)then raise IncompatibleRanges
        else
         if(c1>=0 && c2>=0) then
           match m1,m2 with
            |h1::t1,h2::t2-> (sub_range_helper_l2 h1 h2 b1 d1 b2 d2)::(sub_range_helper_2 t1 t2 (a1,b1) (c1-1,d1) (a2,b2) (c2-1,d2))
            |h1::t1,[]->raise IndexOutofRange
            |[],h2::t2->raise IndexOutofRange
            |[],[]->[]
          else []
         
      
    ;;
    let rec mult_range_helper_l2 (list1:cell list) (list2:cell list) (b1:int) (d1:int) (b2:int) (d2:int):float list=
    if (b1>0) then match list1 with
    | x::xs-> mult_range_helper_l2 xs list2 (b1-1) (d1-1) b2 d2
    | []-> []
    else 
    if (b2>0) then match list2 with
    | x::xs-> mult_range_helper_l2 list1 xs (b1) (d1) (b2-1) (d2-1)
    | []-> []

    else
    if(d1!=d2) then raise IncompatibleRanges
    else 
    if(d1>=0 && d2>=0) then  
     match list1,list2 with
    | Some h1::t1,Some h2::t2-> (h1 *. h2)::(mult_range_helper_l2 t1 t2 b1 (d1-1) b2 (d2-1))
    | Some h1::t1,None::t2->raise EmptyCell
    | None::t1,Some h2::t2->raise EmptyCell
    | None::t1,None::t2 -> raise EmptyCell
    | [],[]-> []
    else []
    ;;
    

    let rec mult_range_helper_2 (m1:matrix2) (m2:matrix2) (a1,b1) (c1,d1) (a2,b2) (c2,d2):float list list =
    
    if(a1>0) then  
      match m1 with
        | x::xs->  (mult_range_helper_2 xs m2 (a1-1,b1) (c1-1,d1) (a2,b2) (c2,d2))
        | []->[]
      else
        if (a2>0) then  
          match m2 with
            | x::xs -> (mult_range_helper_2 m1 xs (a1,b1) (c1,d1) (a2-1,b2) (c2-1,d2))
            | []->[]
        else 
        if(c1!=c2) then raise IncompatibleRanges
        else
         if(c1>=0 && c2>=0) then
           match m1,m2 with
            |h1::t1,h2::t2-> (mult_range_helper_l2 h1 h2 b1 d1 b2 d2)::(mult_range_helper_2 t1 t2 (a1,b1) (c1-1,d1) (a2,b2) (c2-1,d2))
            |h1::t1,[]->raise IndexOutofRange
            |[],h2::t2->raise IndexOutofRange
            |[],[]->[]
          else []
         
      
    ;;
    let rec div_range_helper_l2 (list1:cell list) (list2:cell list) (b1:int) (d1:int) (b2:int) (d2:int):float list=
    if (b1>0) then match list1 with
    | x::xs-> div_range_helper_l2 xs list2 (b1-1) (d1-1) b2 d2
    | []-> []
    else 
    if (b2>0) then match list2 with
    | x::xs-> div_range_helper_l2 list1 xs (b1) (d1) (b2-1) (d2-1)
    | []-> []

    else
    if(d1!=d2) then raise IncompatibleRanges
    else
     if(d1>=0 && d2>=0) then  
     match list1,list2 with
    | Some h1::t1,Some h2::t2-> (h1 /. h2)::(div_range_helper_l2 t1 t2 b1 (d1-1) b2 (d2-1))
    | Some h1::t1,None::t2->raise EmptyCell
    | None::t1,Some h2::t2->raise EmptyCell
    | None::t1,None::t2 -> raise EmptyCell
    | [],[]-> []
    else []
    ;;
    

    let rec div_range_helper_2 (m1:matrix2) (m2:matrix2) (a1,b1) (c1,d1) (a2,b2) (c2,d2):float list list =
    
    if(a1>0) then  
      match m1 with
        | x::xs->  (div_range_helper_2 xs m2 (a1-1,b1) (c1-1,d1) (a2,b2) (c2,d2))
        | []->[]
      else
        if (a2>0) then  
          match m2 with
            | x::xs -> (div_range_helper_2 m1 xs (a1,b1) (c1,d1) (a2-1,b2) (c2-1,d2))
            | []->[]
        else 
        if(c1!=c2) then raise IncompatibleRanges
        else
         if(c1>=0 && c2>=0) then
           match m1,m2 with
            |h1::t1,h2::t2-> (div_range_helper_l2 h1 h2 b1 d1 b2 d2)::(div_range_helper_2 t1 t2 (a1,b1) (c1-1,d1) (a2,b2) (c2-1,d2))
            |h1::t1,[]->raise IndexOutofRange
            |[],h1::t1->raise IndexOutofRange
            |[],[]->[]
          else []
         
      
    ;;
    let rec full_count(m:matrix2) (a,d) (r,s)= assign_m m (full_count_helper_2 m (row_count_helper_2 m a d)) r s;;
    let rec row_count(m:matrix2) (a,d) (r,s)= assign_m_row m (row_count_helper_2 m a d) r s;;
    let rec col_count(m:matrix2) (a,b) (r,s)=  col_count_helper_2 m a b r s;;
    let rec full_sum(m:matrix2) (a,b) (r,s)= assign_m m (full_sum_helper_2 m (row_sum_helper_2 m a b)) r s;;
    let rec row_sum(m:matrix2) (a,b) (r,s)= assign_m_row m (row_sum_helper_2 m a b) r s;;
    let rec col_sum(m:matrix2) (a,b) (r,s)=  col_sum_helper_2 m a b r s;;
    let rec full_avg(m:matrix2) (a,b) (r,s)= assign_m m (calc_avg (full_sum_helper_2 m (row_sum_helper_2 m a b)) (full_count_helper_2 m (row_count_helper_2 m a b))) r s;;
    let rec row_avg(m:matrix2) (a,b) (r,s)= assign_m_row m (calc_avg_row (row_sum_helper_2 m a b) (row_count_helper_2 m a b)) r s;;
    let rec col_avg(m:matrix2) (a,b) (r,s)=  col_avg_helper_2 m a b r s;;;;
    let rec full_min(m:matrix2) (a,b) (r,s)= assign_m m (full_min_helper_2 m (row_min_helper_2 m a b)) r s;; 
    let rec row_min(m:matrix2) (a,b) (r,s)= assign_m_row m (row_min_helper_2 m a b) r s;;
    let rec col_min(m:matrix2) (a,b) (r,s)=  col_min_helper_2 m a b r s;;
    let rec full_max(m:matrix2) (a,b) (r,s)= assign_m m (full_max_helper_2 m (row_max_helper_2 m a b)) r s;;
    let rec row_max(m:matrix2) (a,b) (r,s)= assign_m_row m (row_max_helper_2 m a b) r s;;
    let rec col_max(m:matrix2) (a,b) (r,s)=  col_min_helper_2 m a b r s;;
    let rec add_const(m:matrix2) (a,b) (f:float) (r,s)= write_m m (add_const_helper_2 m f a b) r s;;
    let rec subt_const(m:matrix2) (a,b) (f:float) (r,s)= write_m m (sub_const_helper_2 m f a b) r s;;
    let rec mult_const(m:matrix2) (a,b) (f:float )(r,s)= write_m m (mult_const_helper_2 m f a b) r s;;
    let rec div_const(m:matrix2) (a,b) (f:float )(r,s)= write_m m (div_const_helper_2 m f a b) r s;;
    let rec add_range(m:matrix2) (a,b) (c,d) (r,s)= write_m m (add_range_helper_2 m m a b c d) r s;;
    let rec subt_range(m:matrix2) (a,b) (c,d) (r,s)=  write_m m (sub_range_helper_2 m m a b c d) r s;;
    let rec mult_range(m:matrix2) (a,b) (c,d) (r,s)=  write_m m (mult_range_helper_2 m m a b c d) r s;;
    let rec div_range(m:matrix2) (a,b) (c,d) (r,s)=  write_m m (div_range_helper_2 m m a b c d) r s;;
   
    
;;

# 674 "parser.ml"
let yytransl_const = [|
  257 (* ADD *);
  258 (* SUBT *);
  259 (* MULT *);
  260 (* DIV *);
  261 (* SUM *);
  262 (* ROWSUM *);
  263 (* COLSUM *);
  264 (* AVG *);
  265 (* ROWAVG *);
  266 (* COLAVG *);
  267 (* MIN *);
  268 (* ROWMIN *);
  269 (* COLMIN *);
  270 (* MAX *);
  271 (* ROWMAX *);
  272 (* COLMAX *);
  273 (* COUNT *);
  274 (* ROWCOUNT *);
  275 (* COLCOUNT *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LBRAC *);
  279 (* RBRAC *);
  280 (* COMMA *);
  281 (* COLON *);
  282 (* SEMICOLON *);
  283 (* ASS *);
  286 (* EOL *);
    0|]

let yytransl_block = [|
  284 (* INT *);
  285 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\004\000\003\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\002\000\
\004\000\005\000\039\000\000\000\022\000\030\000\018\000\026\000\
\034\000\023\000\031\000\019\000\027\000\035\000\024\000\032\000\
\020\000\028\000\036\000\025\000\033\000\021\000\029\000\037\000\
\000\000\000\000\038\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\034\000"

let yysindex = "\011\000\
\249\254\000\000\253\254\000\000\246\254\000\255\006\255\000\000\
\070\255\023\255\240\254\241\254\244\254\245\254\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\042\255\249\254\032\255\
\032\255\255\254\032\255\032\255\002\255\032\255\032\255\071\255\
\032\255\032\255\072\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\249\254\046\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\043\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\245\255\021\000"

let yytablesize = 101
let yytable = "\033\000\
\036\000\039\000\042\000\031\000\031\000\003\000\003\000\031\000\
\031\000\003\000\003\000\001\000\032\000\035\000\003\000\008\000\
\038\000\041\000\031\000\060\000\003\000\031\000\064\000\003\000\
\007\000\069\000\009\000\063\000\074\000\010\000\068\000\079\000\
\037\000\040\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\030\000\031\000\061\000\062\000\065\000\066\000\
\067\000\070\000\071\000\072\000\075\000\076\000\077\000\080\000\
\059\000\081\000\083\000\000\000\003\000\082\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\000\000\031\000\031\000\003\000\003\000\000\000\000\000\
\000\000\000\000\000\000\073\000\078\000"

let yycheck = "\011\000\
\012\000\013\000\014\000\020\001\020\001\022\001\022\001\020\001\
\020\001\022\001\022\001\001\000\029\001\029\001\022\001\026\001\
\029\001\029\001\020\001\031\000\022\001\020\001\034\000\022\001\
\028\001\037\000\027\001\029\001\040\000\024\001\029\001\043\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\028\001\020\001\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\023\001\025\001\021\001\255\255\026\001\081\000\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\255\255\020\001\020\001\022\001\022\001\255\255\255\255\
\255\255\255\255\255\255\029\001\029\001"

let yynames_const = "\
  ADD\000\
  SUBT\000\
  MULT\000\
  DIV\000\
  SUM\000\
  ROWSUM\000\
  COLSUM\000\
  AVG\000\
  ROWAVG\000\
  COLAVG\000\
  MIN\000\
  ROWMIN\000\
  COLMIN\000\
  MAX\000\
  ROWMAX\000\
  COLMAX\000\
  COUNT\000\
  ROWCOUNT\000\
  COLCOUNT\000\
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  COMMA\000\
  COLON\000\
  SEMICOLON\000\
  ASS\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 665 "parser.mly"
                        (_1)
# 844 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 668 "parser.mly"
                                (print_mat (full_count sheet2 _4 _1))
# 852 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'INDEX) in
    Obj.repr(
# 669 "parser.mly"
                            (print_mat sheet2)
# 859 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 670 "parser.mly"
                                    (print_mat (row_count sheet2 _4 _1))
# 867 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 671 "parser.mly"
                                    (print_mat (col_count sheet2 _4 _1))
# 875 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 672 "parser.mly"
                               (print_mat(full_sum sheet2 _4 _1))
# 883 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 673 "parser.mly"
                                  (print_mat(row_sum sheet2 _4 _1))
# 891 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 674 "parser.mly"
                                  (print_mat(col_sum sheet2 _4 _1))
# 899 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 675 "parser.mly"
                               (print_mat(full_avg sheet2 _4 _1))
# 907 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 676 "parser.mly"
                                  (print_mat(row_avg sheet2 _4 _1))
# 915 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 677 "parser.mly"
                                  (print_mat(col_avg sheet2 _4 _1))
# 923 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 678 "parser.mly"
                               (print_mat(full_min sheet2 _4 _1))
# 931 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 679 "parser.mly"
                                  (print_mat(row_min sheet2 _4 _1))
# 939 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 680 "parser.mly"
                                  (print_mat(col_min sheet2 _4 _1))
# 947 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 681 "parser.mly"
                               (print_mat(full_max sheet2 _4 _1))
# 955 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 682 "parser.mly"
                                  (print_mat(row_max sheet2 _4 _1))
# 963 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 683 "parser.mly"
                                  (print_mat(col_max sheet2 _4 _1))
# 971 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 684 "parser.mly"
                                     (print_mat(add_const sheet2 _4 _5 _1))
# 980 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 685 "parser.mly"
                                     (print_mat(subt_const sheet2 _4 _5 _1))
# 989 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 686 "parser.mly"
                                     (print_mat(mult_const sheet2 _4 _5 _1))
# 998 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 687 "parser.mly"
                                     (print_mat(div_const sheet2 _4 _5 _1))
# 1007 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 688 "parser.mly"
                                     (print_mat(add_const sheet2 _5 _4 _1))
# 1016 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 689 "parser.mly"
                                     (print_mat(subt_const sheet2 _5 _4 _1))
# 1025 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 690 "parser.mly"
                                     (print_mat(mult_const sheet2 _5 _4 _1))
# 1034 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 691 "parser.mly"
                                     (print_mat(div_const sheet2 _5 _4 _1))
# 1043 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'INDEX) in
    Obj.repr(
# 692 "parser.mly"
                                     (print_mat(add_const sheet2 _4 (value sheet2 _5) _1))
# 1052 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'INDEX) in
    Obj.repr(
# 693 "parser.mly"
                                     (print_mat(subt_const sheet2 _4 (value sheet2 _5) _1))
# 1061 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'INDEX) in
    Obj.repr(
# 694 "parser.mly"
                                     (print_mat(mult_const sheet2 _4 (value sheet2 _5) _1))
# 1070 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'INDEX) in
    Obj.repr(
# 695 "parser.mly"
                                     (print_mat(div_const sheet2 _4 (value sheet2 _5) _1))
# 1079 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'INDEX) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 696 "parser.mly"
                                     (print_mat(add_const sheet2 _5 (value sheet2 _4) _1))
# 1088 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'INDEX) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 697 "parser.mly"
                                     (print_mat(subt_const sheet2 _5 (value sheet2 _4) _1))
# 1097 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'INDEX) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 698 "parser.mly"
                                     (print_mat(mult_const sheet2 _5 (value sheet2 _4) _1))
# 1106 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'INDEX) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 699 "parser.mly"
                                     (print_mat(div_const sheet2 _5 (value sheet2 _4) _1))
# 1115 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 700 "parser.mly"
                                     (print_mat(add_range sheet2 _4 _5 _1))
# 1124 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 701 "parser.mly"
                                     (print_mat(subt_range sheet2 _4 _5 _1))
# 1133 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 702 "parser.mly"
                                     (print_mat(mult_range sheet2 _4 _5 _1))
# 1142 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'RANGE) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'RANGE) in
    Obj.repr(
# 703 "parser.mly"
                                     (print_mat(div_range sheet2 _4 _5 _1))
# 1151 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'INDEX) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'INDEX) in
    Obj.repr(
# 706 "parser.mly"
                                          (((_2,_4)))
# 1159 "parser.ml"
               : 'RANGE))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 709 "parser.mly"
                                    (((_2,_4)))
# 1167 "parser.ml"
               : 'INDEX))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
;;
# 712 "parser.mly"

 


      

            
# 1200 "parser.ml"
