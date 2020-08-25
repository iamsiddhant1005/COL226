open List
type symbol =  string * int;;
type variable = string;;
type signature= symbol list;;
type term = V of variable | Node of (symbol * (term list));;
type substitution = term * term;;
type clause = term * term list
exception TooDeep

let a:term = Node(("mother",0),[V("x");V("y")]);;
let b:term = Node(("mother",0),[Node(("ala",0),[]);Node(("aba",0),[])]);;

let rec printTable l = match l with x::xs -> (printClause x);Printf.printf "NEXT CLAUSE \n\n\n\n"; (printTable xs);
| [] -> Printf.printf "EMPTY"
and printClause c = match c with (ter, terl) -> Printf.printf "Clause Start XXXXXXXXXXX\n"; (printTerm ter); Printf.printf "\nXXXXXXXXX MAPPED TO \n"; (printTerms terl);
and printTerms tl = match tl with [] -> Printf.printf ""; | x::xs -> (printTerm x);(Printf.printf "next term\n");(printTerms xs);
and printTerm t = match t with V((str)) ->  Printf.printf "Variable %s\n" str;
|Node((str,integ),children) -> Printf.printf "Symbol %s,%d\n Children\n" str integ; (printTerms children);Printf.printf "ChildrenOver\n";
;;
exception InvalidSubstition
exception NOT_UNIFIABLE


let rec print_symbol (a,b)= print_string a;;
let rec print_term exp=
 match exp with
 | V(x)-> print_string x;
 | Node(a,[])->print_symbol a;
 | Node(a,(lst:term list))->print_string "Node(";print_symbol a;print_string",";print_string"[";(List.iter print_term lst);print_string"]";print_string ")"
;;

let rec check_arity (lst:symbol list):bool=
match lst with
| (a,b)::[]->if(b>=0)then true else false
| (a,b)::t-> if(b>=0)then check_arity t else false
| []->true

;;
let rec check_list2 (lst:symbol list) (a,b):bool=
match lst with
|(c,d)::t-> if(c=a) then false else true
|[]->true
;;

let rec check_rep (list1:symbol list) (list2:symbol list):bool = 
match list1 with
| (a,b)::[]->if((check_list2 list2 (a,b))=true) then true else false
| (a,b)::t-> if((check_list2 list2 (a,b))=true) then (check_rep t ([(a,b)]@list2)) else false
| []->false
;;
let rec print_bool (b:bool):string= 
 if(b=true)then "true" else "false"
 ;;
(*let rec check_sig(sign_list:symbol list):bool = (check_rep sign_list checked_symbol)&&(check_arity sign_list);;*)



let rec lst_dim (lst:term list)=
match lst with
| V(x)::t->1+lst_dim t
| Node(a,(x:term list))::t-> 1 + lst_dim t
| []-> 0
;;

let rec lst_dim2 lst=
match lst with
|h::t-> 1 + lst_dim2 t
| []->0 
;;



let rec compare_list_size (exp:term) (a,b) (size:int)= if(b=size) then true else false;;


let rec and_bool (b:bool) (lst:bool list):bool = 
	if(b=false) then false
	else match lst with
	| h::t-> if(h=true) then and_bool b t else false
	|[]-> true 
;;
let rec check_size (exp:term):bool =
	match exp with
	| V(x)->true
	| Node(a,[])-> compare_list_size exp a 0
	| Node(a,(lst:term list))-> and_bool (compare_list_size exp a (lst_dim lst)) (List.map check_size lst)
	
;;
let rec find_in_sig  (sign:signature) (exp:term):bool=
match exp with
|V(x)->true
|Node(a,[])->(List.mem a sign)
|Node(a,(lst:term list))->if List.mem a sign then and_bool true (List.map (find_in_sig sign) lst) else false

;;
let rec wfterm (exp:term) (sign:signature):bool=(check_size exp)&&(find_in_sig sign exp);;



let max_int (a:int) (b:int):int= if(a>=b) then a else b;;


let rec max_ht (lst:int list):int = 
 match lst with
 | h::t-> max_int h (max_ht t)
 | []-> (-100000000)
;;
let rec check_ht (exp:term):int =
 match exp with
 | V(x)->0
 | Node(a,[])-> 0
 | Node(a,(lst:term list))-> 1 + max_ht (List.map check_ht lst)

;;

let rec ht (exp:term):int=check_ht exp;;

let rec sum_list (lst:int list):int=
match lst with
| h::t->h + sum_list t
| []->0
;;
let rec calc_size (exp:term):int =
 match exp with
 | V(x)->1
 | Node(a,[])-> 1
 | Node(a,(lst:term list))-> 1 + sum_list (List.map calc_size lst)

;;


let rec size (exp:term):int= calc_size exp;;

let rec print_list (lst:string list)=
match lst with
| x::t-> print_string x ; print_string " ";print_list t 
| []-> print_string " "
;;



let rec make_list (lst:string list list):string list=
match lst with
|h::t->h@make_list t
|[]::t->make_list t
| []->[]
;;
let rec check_vars (exp:term):string list=
match exp with
|V(x)-> [x]
| Node(a,[])->[]
| Node(a,(lst:term list))->  make_list (List.map check_vars lst)
;;
let make_unique (x:string) (xs:string list):string list= if List.mem x xs then xs else x::xs;; 

let remove_duplicate (lst:string list)= (List.fold_right make_unique lst [])  
let rec vars (exp:term):string list= remove_duplicate(check_vars exp);;



let rec compare_var (V(x):term) (V(y):term)=if(x=y) then true else false;;
let rec compare_symbol (a,b) (c,d):bool= if(a=c) then true else false
let rec make_term_list lst:term list=
match lst with
|h::t->h::make_term_list t
|[]->[]
;;
let rec make_sub (a,b) (exp:term):term =
match exp with
|V(x)-> if compare_var a exp then b else exp
|Node(a,[])->exp
|Node(x,lst)-> Node(x,make_term_list(List.map (make_sub (a,b)) lst))  
;;
let rec subst (exp:term) (sub:substitution):term = make_sub sub exp;;

let rec make_sub_list (exp:term) (sub_list:substitution list)=
match sub_list with
| h::t-> make_sub_list (subst exp h) t
| []->exp
;;
let rec or_bool (b:bool) (lst:bool list):bool = 
	if(b=true) then true
	else match lst with
	| h::t-> if(h=false) then and_bool b t else true
	|[]-> false
;;
let rec take_subst_list (lst:substitution list):substitution=
match lst with
| (a,b)::[]->(a,b)
|(a,b)::t->if (a<>b) then (a,b) else take_subst_list t

;;
let rec print_subst_list (lst:substitution list) list2=
match lst with
|(V(x),b)::[]->if (List.mem x list2) then  (print_string "(";print_term (V(x));print_string ":";print_term b; print_string ")";print_string ";"; print_subst_list [] list2) else print_subst_list [] list2
|(V(x),b)::t->if (List.mem x list2) then  (print_string "(";print_term (V(x));print_string ":";print_term b; print_string ")";print_string ";";print_string" "; print_subst_list t list2) else print_subst_list t list2
| []-> print_string ""
;;

let rec print_subst_list_2 (lst:substitution list list) list2=
match lst with
|h::t-> (*print_string"["*)print_subst_list h list2;(*print_string"]"*)print_string" ";print_subst_list_2 t list2
| []-> print_string ""
;;



let rec find_mgu (exp1:term) (exp2: term) :substitution= 
match exp1,exp2 with
| V(x),V(y)-> if(x=y) then (exp1,exp1)else (exp1,exp2) 
| V(x),Node(a,[])-> (exp1,exp2)
| V(x),Node(a,(lst:term list))-> if List.mem x (vars exp2) then raise NOT_UNIFIABLE else (exp1,exp2)
| Node(a,[]),V(x)-> (exp2,exp1)
| Node(a,[]),Node(b,[])->if compare_symbol a b then (exp1,exp1) else raise NOT_UNIFIABLE
| Node(a,[]),Node(b,(lst:term list))->raise NOT_UNIFIABLE
| Node(a,(lst:term list)),V(x)-> if List.mem x (vars exp1) then raise NOT_UNIFIABLE else (exp2,exp1)
| Node(a,(lst:term list)),Node(b,[])->raise NOT_UNIFIABLE
| Node(a,(list1:term list)),Node(b,(list2:term list))-> if compare_symbol a b then take_subst_list(List.map2 find_mgu list1 list2) else raise NOT_UNIFIABLE

;;


let rec mgu (exp1:term) (exp2:term): substitution list=
 if (exp1<>exp2) then (find_mgu exp1 exp2)::(mgu (subst exp1 (find_mgu exp1 exp2)) (subst exp2 (find_mgu exp1 exp2))) else [] 

;;

 
let  check_mgu exp1 exp2:bool = 
 let s= try mgu exp1 exp2 with NOT_UNIFIABLE->[(V("NOT_UNIFIABLE"),V("NOT_UNIFIABLE"))] in
 match s with 
 | (V("NOT_UNIFIABLE"),V("NOT_UNIFIABLE"))::t->false
 | _->true
;;
let rec unify exp1 exp2 = if exp1<>exp2 then unify (subst exp1 (find_mgu exp1 exp2)) (subst exp2 (find_mgu exp1 exp2)) else exp2;;









let rec subst_goal_list goal_lst (subst_list:substitution list):term list=
match goal_lst with
| h::t-> (make_sub_list h subst_list):: subst_goal_list t subst_list
|[]->[]
;;

let rec solveGoalHelper_bool table table1 goal: bool =
match goal with 
|Node(y,(list2:term list))->

match table with 
| ((exp:term),[])::xs-> if exp<>(Node(y,list2)) then solveGoalHelper_bool xs table1 goal else true
						
												
| (Node(x,(lst:term list)),goal_lst)::xs-> if x<>y then  solveGoalHelper_bool xs table1 goal else and_bool true (List.map (solveGoalHelper_bool table1 table1) (subst_goal_list goal_lst (mgu (Node(x,lst)) goal))) 
													

| []->false



;;







let rec make_list_2 lst=
match lst with
|h::t->h::make_list_2 t
|[]::t->make_list_2 t
| []->[]
;;


let rec solve_conds table (visited:substitution list) goal_lst:substitution list list=
match goal_lst with
|h::t-> if (lst_dim2 (vars h))=0 then 
								 if solveGoalHelper_bool table table h then solve_conds table visited t else []
		else
		begin
		let rec fboo (s:substitution list list) :substitution list list=
		match s with
		    |h1::t1-> (solve_conds table (visited@h1) (subst_goal_list t h1))@(fboo t1)
		    | []->[]
		
		 in fboo (check_table table table h) 	
		 end		 
|[]->[visited]

and check_table table table1 goal=
match goal with
|Node(y,(list2:term list))->
match table with
|(Node(x,lst),[])::xs-> if x<>y then (check_table xs table1 goal) else if check_mgu (Node(x,lst)) (Node(y,list2)) then (mgu (Node(x,lst)) (Node(y,list2)))::(check_table xs table1 goal)  else (check_table xs table1 goal)
|(Node(x,(lst:term list)),goal_list2)::xs->if x<>y then (check_table xs table1 goal) else solve_conds table1 [] (subst_goal_list goal_list2 (mgu (Node(x,lst)) goal))
|[]->[]
;;








let rec solveGoalHelper_list table table1 goal: substitution list list=
match goal with
|Node(y,(list2:term list))->
match table with
|(Node(x,lst),[])::xs-> if x<>y then solveGoalHelper_list xs table1 goal else (check_table table1 table1 goal)
| (Node(x,(lst:term list)),goal_lst)::xs-> if x<>y then solveGoalHelper_list xs table1 goal else if (lst_dim2 (make_list (List.map vars (subst_goal_list goal_lst (mgu (Node(x,lst)) goal)))))=0 then 
																										if (and_bool true (List.map (solveGoalHelper_bool table1 table1) (subst_goal_list goal_lst (mgu (Node(x,lst)) goal)))) then [[]]
																										else []
																							     else (solve_conds table1 [] (subst_goal_list goal_lst (mgu (Node(x,lst)) goal)))
| []->[]
;;
let rec return_subst_head lst printed= 
match lst with 
|h::t->if (List.mem h printed) then return_subst_head t printed else h
|[]->print_string("No");[]
;;
let decide_to_continue a =
	let rea = read_line() in
	match rea with 
	|";"->true
	|"."->false
	|_->Printf.printf"Invalid Symbol\n";false
;;

let rec solveGoal table goal printed= if (lst_dim2 (vars goal))=0 then if (lst_dim2 (solveGoalHelper_list table table goal))=0 then print_string("No") else print_string("Yes")
							else if (lst_dim2 (solveGoalHelper_list table table goal))=0 then print_string("No") else let ans_lst= (return_subst_head (solveGoalHelper_list table table goal) printed) in 
																																	(print_subst_list_2 [ans_lst] (vars goal));print_string("\n");print_string("?-");
																																	if (decide_to_continue true) then (solveGoal table goal (ans_lst::printed)) else flush stdout
;;
(*print_subst_list(check_table table table (Node(("father",2),[V("X");V("K")])));;*)