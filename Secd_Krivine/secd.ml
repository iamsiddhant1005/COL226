type exp = Var of string
		|  Num of int
		|  Boolean of bool
		|  Lambda of (string * exp)
		|  Call of (exp * exp)
		|  Add of (exp * exp)
		|  Mult of (exp * exp)
		|  Subt of (exp * exp)
		|  Div of (exp * exp)
		|  And of (exp * exp)
		|  Or of (exp * exp)
		|  LessThan of (exp * exp)
		|  GreaterThan of (exp * exp)
		|  GreaterThanEqual of (exp * exp)
		|  LessThanEqual of (exp * exp)
		|  Let_in_end of (string * exp * exp)
		|  If_then_else of (exp * exp * exp)
;;


type bind = BNum of (string * int) | Bbool of (string * bool);;
type table = bind list;;

type opcode = Lookup of string
			| LDC of int
			| LDB of bool
			| BIND of string
			| PLUS
			| MULT
			| SUBT
			| DIV 
			| AND
			| OR 
			| LT 
			| GT 
			| LTE
			| GTE
			| LET of string
			| END
			| IF of (opcode list * opcode list)
			| THEN 
			| ELSE
			| CALL
			| LAMBDA of (string * opcode list)
			| RET
;;

type ans = Num of int | Boolean of bool | LAMBDAEX of (string * bind list * opcode list);;
type dump =(ans list)*(bind list)*(opcode list);;
let test:exp = Call(Lambda("y",Num(5)),Div(Num(0),Num(6)));;

let rec print_ans (a:ans) = match a with
| Num(e) -> print_int e
| Boolean(b) -> if b = true then print_string ("True") else print_string("False")
;;


let rec compile (test_exp:exp): opcode list = 
match test_exp with
| Var(e) -> [Lookup(e)]
| Num(e) ->	[LDC(e)] 
| Boolean(e) -> [LDB(e)]
| Add(e1,e2) -> (compile e1)@(compile e2)@([PLUS])
| Mult(e1,e2) -> (compile e1)@(compile e2)@([MULT])
| Subt(e1,e2) -> (compile e1)@(compile e2)@([SUBT])
| Div(e1,e2) -> (compile e1)@(compile e2)@([DIV])
| And(e1,e2) -> (compile e1)@(compile e2)@([AND])
| Or(e1,e2) -> (compile e1)@(compile e2)@([OR])
| GreaterThanEqual(e1,e2) -> (compile e1)@(compile e2)@([GTE])
| LessThanEqual(e1,e2) -> (compile e1)@(compile e2)@([LTE])
| GreaterThan(e1,e2) -> (compile e1)@(compile e2)@([GT])
| LessThan(e1,e2) -> (compile e1)@(compile e2)@([LT])
| Let_in_end(s,e1,e2) -> (compile e1)@[LET(s)]@(compile e2)@[END]
| If_then_else(e1,e2,e3) -> (compile e1)@[IF((compile e2)@[THEN],(compile e3)@[ELSE])]
| Call(e1,e2) -> (compile e1)@(compile e2)@([CALL])
| Lambda(s,e2) -> [LAMBDA(s,(compile e2)@[RET])]   


;;

let rec find_var (table:bind list) (v:string):ans = 
match table with
| BNum(str,n)::t-> if v<>str then find_var t v else Num(n)
| Bbool(str,b)::t -> if v<>str then find_var t v else Boolean(b)
;;

let rec secd_mc (st:ans list) (table:bind list) (compile_lst:opcode list) (dump:dump list):ans = 
match st,table,compile_lst,dump with 
|  (s1::s2,e1,[],d1) -> s1 
|  (s1,e1,LDC(e)::c2,d1) -> secd_mc (Num(e)::s1) e1 c2 d1
|  (s1,e1,LDB(e)::c2,d1) -> secd_mc (Boolean(e)::s1) e1 c2 d1
|  (s1,e1,Lookup(e)::c2,d1) -> secd_mc ((find_var e1 e)::s1) e1 c2 d1
|  (s1,e1,LAMBDA(str,c)::c2,d1) -> secd_mc (LAMBDAEX(str,e1,c)::s1) e1 c2 d1
|  (Num(e)::LAMBDAEX(str,e1,c)::s2,e2,CALL::c2,d2) -> secd_mc [] ([BNum(str,e)]@e1) c ((s2,e2,c2)::d2)
|  (Boolean(e)::LAMBDAEX(str,e1,c)::s2,e2,CALL::c2,d2) -> secd_mc [] ([Bbool(str,e)]@e1) c ((s2,e2,c2)::d2) 
|  (Num(a1)::Num(a2)::s1,e1,PLUS::c1,d1) -> secd_mc (Num(a1+a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,MULT::c1,d1) -> secd_mc (Num(a1*a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,SUBT::c1,d1) -> secd_mc (Num(a1-a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,DIV::c1,d1) -> secd_mc (Num(a1/a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,LT::c1,d1) -> secd_mc (Boolean(a1>a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,GT::c1,d1) -> secd_mc (Boolean(a1<a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,LTE::c1,d1) -> secd_mc (Boolean(a1>=a2)::s1) e1 c1 d1
|  (Num(a1)::Num(a2)::s1,e1,GTE::c1,d1) -> secd_mc (Boolean(a1<=a2)::s1) e1 c1 d1
|  (Boolean(a1)::Boolean(a2)::s1,e1,AND::c1,d1) -> secd_mc (Boolean(a1 && a2)::s1) e1 c1 d1
|  (Boolean(a1)::Boolean(a2)::s1,e1,OR::c1,d1) -> secd_mc (Boolean(a1 || a2)::s1) e1 c1 d1
|  (Num(e)::s1, e1, LET(str)::c1,d1 ) -> secd_mc s1 ([BNum(str,e)]@e1) c1 ((s1,e1,c1)::d1)
|  (Boolean(e)::s1, e1, LET(str)::c1,d1 ) -> secd_mc s1 ([Bbool(str,e)]@e1) c1 ((s1,e1,c1)::d1)
|  (s2,e2,END::c2, (s1,e1,c1)::d2) -> secd_mc s2 e1 c2 d2 
|  (Boolean(b)::s1,e1,IF(a1,a2)::c1,d1)-> if b = true then secd_mc s1 e1 (a1@c1) d1 else secd_mc s1 e1 (a2@c1) d1
|  (s1,e1,THEN::c1 ,d1)-> secd_mc s1 e1 c1 d1
|  (s1,e1,ELSE::c1 ,d1)-> secd_mc s1 e1 c1 d1
|  (h::t,e2,RET::c2,(s1,e1,c1)::d2) -> secd_mc (h::s1) e1 c1 d2 
;;
 
print_ans(secd_mc [] [] (compile test) []);;