%{
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

%} 

        
        %token ADD SUBT MULT DIV
        %token SUM ROWSUM COLSUM 
        %token AVG ROWAVG COLAVG
        %token MIN ROWMIN COLMIN
        %token MAX ROWMAX COLMAX
        %token COUNT ROWCOUNT COLCOUNT
        %token LPAREN RPAREN
        %token LBRAC RBRAC
        %token COMMA
        %token COLON
        %token SEMICOLON
        %token ASS
        %token <int> INT     
        %token <float> FLOAT
        
        %token EOL
         
        %start main
        %type<string>main 
              
        
        %%
        main:
        | exp SEMICOLON {$1}
      ;
        exp:
         INDEX ASS COUNT RANGE  {print_mat (full_count sheet2 $4 $1)}
         | INDEX            {print_mat sheet2}
        | INDEX ASS ROWCOUNT RANGE  {print_mat (row_count sheet2 $4 $1)}
        | INDEX ASS COLCOUNT RANGE  {print_mat (col_count sheet2 $4 $1)}
        | INDEX ASS SUM RANGE  {print_mat(full_sum sheet2 $4 $1)}
        | INDEX ASS ROWSUM RANGE  {print_mat(row_sum sheet2 $4 $1)}
        | INDEX ASS COLSUM RANGE  {print_mat(col_sum sheet2 $4 $1)}
        | INDEX ASS AVG RANGE  {print_mat(full_avg sheet2 $4 $1)}
        | INDEX ASS ROWAVG RANGE  {print_mat(row_avg sheet2 $4 $1)}
        | INDEX ASS COLAVG RANGE  {print_mat(col_avg sheet2 $4 $1)}
        | INDEX ASS MIN RANGE  {print_mat(full_min sheet2 $4 $1)}
        | INDEX ASS ROWMIN RANGE  {print_mat(row_min sheet2 $4 $1)}
        | INDEX ASS COLMIN RANGE  {print_mat(col_min sheet2 $4 $1)}
        | INDEX ASS MAX RANGE  {print_mat(full_max sheet2 $4 $1)}
        | INDEX ASS ROWMAX RANGE  {print_mat(row_max sheet2 $4 $1)}
        | INDEX ASS COLMAX RANGE  {print_mat(col_max sheet2 $4 $1)}
        | INDEX ASS ADD RANGE FLOAT  {print_mat(add_const sheet2 $4 $5 $1)}
        | INDEX ASS SUBT RANGE FLOAT {print_mat(subt_const sheet2 $4 $5 $1)}
        | INDEX ASS MULT RANGE FLOAT {print_mat(mult_const sheet2 $4 $5 $1)}
        | INDEX ASS DIV RANGE FLOAT  {print_mat(div_const sheet2 $4 $5 $1)}
        | INDEX ASS ADD FLOAT RANGE  {print_mat(add_const sheet2 $5 $4 $1)}
        | INDEX ASS SUBT FLOAT RANGE {print_mat(subt_const sheet2 $5 $4 $1)}
        | INDEX ASS MULT FLOAT RANGE {print_mat(mult_const sheet2 $5 $4 $1)}
        | INDEX ASS DIV FLOAT RANGE  {print_mat(div_const sheet2 $5 $4 $1)}
        | INDEX ASS ADD RANGE INDEX  {print_mat(add_const sheet2 $4 (value sheet2 $5) $1)}
        | INDEX ASS SUBT RANGE INDEX {print_mat(subt_const sheet2 $4 (value sheet2 $5) $1)}
        | INDEX ASS MULT RANGE INDEX {print_mat(mult_const sheet2 $4 (value sheet2 $5) $1)}
        | INDEX ASS DIV RANGE INDEX  {print_mat(div_const sheet2 $4 (value sheet2 $5) $1)}
        | INDEX ASS ADD INDEX RANGE  {print_mat(add_const sheet2 $5 (value sheet2 $4) $1)}
        | INDEX ASS SUBT INDEX RANGE {print_mat(subt_const sheet2 $5 (value sheet2 $4) $1)}
        | INDEX ASS MULT INDEX RANGE {print_mat(mult_const sheet2 $5 (value sheet2 $4) $1)}
        | INDEX ASS DIV INDEX RANGE  {print_mat(div_const sheet2 $5 (value sheet2 $4) $1)}
        | INDEX ASS ADD RANGE RANGE  {print_mat(add_range sheet2 $4 $5 $1)}
        | INDEX ASS SUBT RANGE RANGE {print_mat(subt_range sheet2 $4 $5 $1)}
        | INDEX ASS MULT RANGE RANGE {print_mat(mult_range sheet2 $4 $5 $1)}
        | INDEX ASS DIV RANGE RANGE  {print_mat(div_range sheet2 $4 $5 $1)}
        
        RANGE:
        | LPAREN INDEX COLON INDEX RPAREN {(($2,$4))}

        INDEX:
        | LBRAC INT COMMA INT RBRAC {(($2,$4))}
      ;  
        %%

 


      

            