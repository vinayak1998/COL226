(*
    Vinayak Rastogi

    2016CS10345

    COL226

    Programming Languages (Spring - 2018)

    Assignment #4 :- Abstract Machines (SECD + Krivine)

*)

(*
    Consider a tiny language consisting of expressions that are

    e ::= x | \x.e_1 | (e_1 e_2)

    where \x.e_1 is to be read as "\lambda x. e_1"

    {

    In addition, to make things interesting, you can add the booleans T and F, and an if_then_else expression.  And tuples.

    You can also introduce the natural numbers, addition (perhaps multiplication) and comparisons (=, >)

    }



    For the language you take, you should design and implement (in OCaml) the following abstract machines

    1.  The Krivine Machine (in closure form), that implements Call-by-Name semantics.

    For this you need to consider Closures as a pair of a Table and an expression, where a Table is a partial function from variables to Answers (including value closures).



    2. The SECD machine that implements Call-by-Value semantics.

    For this you need value closures in the set of answers.

    You also need to implement the compile function.
*)

type exp =
    Const of int
  | Abs of exp
  | Identifier of string
  | Plus of exp * exp
  | Sub of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | Exp of exp * exp
  | T
  | F
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | Impl of exp * exp
  | Equal of exp * exp
  | Gt of exp * exp
  | Lt of exp * exp
  | Goe of exp * exp
  | Loe of exp * exp
  | Lambda of string*exp
  | Function of exp*exp;;

type opcode = CONST of int
  | ABS
  | IDENTIFIER of string
  | PLUS
  | SUB
  | MULT
  | DIV
  | MOD
  | EXP
  | TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | IMPL
  | EQUAL
  | GT
  | LT
  | GOE
  | LOE
  | CLOS of string*opcode list
  | RET
  | APP;;


type table = (string*answer) list
and answer = Constans of int|Boolans of bool | Closure of table*string*(opcode list) | Vclosure of table*exp;;

let abso e = match e with Const(n) -> Constans(abs n);;

let anstoint t = match t with
Constans n -> n;;

let anstobool t = match t with
Boolans n -> n;;

let imply e = match e with
(true,false) -> false
| _ -> true;;

let exptoans t = match t with
Const n -> Constans n
|T -> Boolans true
|F -> Boolans false;;

let rec compile e = match e with
 Const n -> [CONST(n)]
|Abs n-> (compile n)@[ABS]
|Identifier n -> [IDENTIFIER(n)]
|Plus (n1,n2) -> (compile n1)@(compile n2)@[PLUS]
|Sub (n1,n2) -> (compile n1)@(compile n2)@[SUB]
|Mult (n1,n2) -> (compile n1)@(compile n2)@[MULT]
|Div (n1,n2) -> (compile n1)@(compile n2)@[DIV]
|Mod (n1,n2) -> (compile n1)@(compile n2)@[MOD]
|Exp (n1,n2) -> (compile n1)@(compile n2)@[EXP]
|T -> [TRUE]
|F -> [FALSE]
|Not (n1) -> (compile n1)@[NOT]
|And (n1,n2) -> (compile n1)@(compile n2)@[AND]
|Or (n1,n2) -> (compile n1)@(compile n2)@[OR]
|Impl (n1,n2) -> (compile n1)@(compile n2)@[IMPL]
|Equal (n1,n2) -> (compile n1)@(compile n2)@[EQUAL]
|Gt (n1,n2) -> (compile n1)@(compile n2)@[GT]
|Lt (n1,n2) -> (compile n1)@(compile n2)@[LT]
|Goe (n1,n2) -> (compile n1)@(compile n2)@[GOE]
|Loe (n1,n2) -> (compile n1)@(compile n2)@[LOE]
|Lambda (str,expr) -> [CLOS(str,compile(expr)@[RET])]
|Function (e1,e2) -> compile(e1) @ compile(e2) @ [APP];;

let absoexe e = match e with Constans(n) -> Constans(abs n);;

open List;;

let first(e1,e2) = e1;;

let second(e1,e2) = e2;;

let rec buildfirst l = match l with
    [] -> []
|   x::xs -> [(first x)] @ ( buildfirst xs);;

let rec buildsecond l = match l with
    [] -> []
|   x::xs -> [(second x)] @ ( buildsecond xs);;

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t;;

let map t n = second (List.nth t (find n (buildfirst t)));;

let rec execute(s,table,c,d) = match (s,table,c,d) with
(s,table,[],d) -> hd s
|(s,table,CONST(n)::c',d) -> execute(Constans(n)::s , table, c',d)
|(n1::n2::s,table,PLUS::c',d) -> execute(Constans ((anstoint n1) + (anstoint n2))::s , table ,c',d)
|(n1::n2::s,table,SUB::c',d) -> execute(Constans ((anstoint n2) - (anstoint n1))::s, table ,c',d)
|(n1::n2::s,table,MULT::c',d) -> execute(Constans ((anstoint n1) * (anstoint n2))::s, table,c',d)
|(n1::n2::s,table,DIV::c',d) -> execute(Constans ((anstoint n2) / (anstoint n1))::s,table,c',d)
|(n1::n2::s,table,MOD::c',d) -> execute(Constans ((anstoint n2) mod (anstoint n1))::s,table,c',d)
|(n1::n2::s,table,EXP::c',d) -> execute(Constans (int_of_float ( ( float_of_int(anstoint n2) ) ** ( float_of_int(anstoint n1) ) ))::s,table,c',d)
|(n::s,table,ABS::c',d) -> execute(absoexe(n)::s, table, c',d)
|(s, table, IDENTIFIER(n)::c',d ) -> execute((map table n)::s, table, c',d)
|(s, table, TRUE::c',d) -> execute(Boolans true::s, table, c' ,d)
|(s, table, FALSE::c',d) -> execute(Boolans false::s, table, c' ,d)
|(n::s , table , NOT::c',d) -> execute( Boolans (not (anstobool n)) ::s,table, c',d)
|(n1::n2::s, table, AND::c',d) -> execute(Boolans ((anstobool n2) && (anstobool n1))::s, table, c',d)
|(n1::n2::s, table, OR::c',d) -> execute(Boolans ((anstobool n2) || (anstobool n1))::s, table, c',d)
|(n1::n2::s, table, IMPL::c',d) -> execute(Boolans (imply (anstobool n2,anstobool n1))::s, table, c',d)
|(n1::n2::s, table, EQUAL::c',d) -> execute(Boolans (anstoint n2 = anstoint n1)::s, table, c',d)
|(n1::n2::s, table, GT::c',d) -> execute(Boolans (anstoint n2 > anstoint n1)::s, table, c',d)
|(n1::n2::s, table, LT::c',d) -> execute(Boolans (anstoint n2 < anstoint n1)::s, table, c',d)
|(n1::n2::s, table, GOE::c',d) -> execute(Boolans (anstoint n2 >= anstoint n1)::s, table, c',d)
|(n1::n2::s, table, LOE::c',d) -> execute(Boolans (anstoint n2 <= anstoint n1)::s, table, c',d)
|(s,table,CLOS(y,c)::c',d) -> execute(Closure(table,y,c)::s,table,c',d)
|(a::Closure(table,y,c)::s, table', APP::c' , d) -> execute([],(y,a)::table', c , (s,table,c')::d)
|(a::s',table',RET::c',(s,table,c)::d) -> execute(a::s,table,c',d);;

let e = Function(Lambda("x",Plus(Identifier("x"),Const(5))),Const(4));;

let c = compile e;;

execute([],[],c,[]);;



let rec eval map rho t = match t with
Const n -> Constans n
|Abs n -> abso n
|Identifier n -> map rho n
|Plus(n1,n2) -> Constans ( anstoint (eval map rho n1) + anstoint (eval map rho n2) )
|Sub(n1,n2) -> Constans ( anstoint (eval map rho n1) - anstoint (eval map rho n2) )
|Mult(n1,n2) -> Constans ( anstoint (eval map rho n1) * anstoint (eval map rho n2) )
|Div(n1,n2) -> Constans ( anstoint (eval map rho n1) / anstoint (eval map rho n2) )
|Mod(n1,n2) -> Constans ( anstoint (eval map rho n1) mod anstoint (eval map rho n2) )
|Exp(n1,n2) -> Constans (int_of_float ( ( float_of_int(anstoint (eval map rho n1)) ) ** ( float_of_int(anstoint (eval map rho n2)) ) ))
|T -> Boolans true
|F -> Boolans false
|Not n -> Boolans (not (anstobool(eval map rho n)))
|And (n1,n2) -> Boolans   (((anstobool (eval map rho n1))) && (anstobool (eval map rho n1)))
|Or (n1,n2) -> Boolans    (((anstobool (eval map rho n1))) || (anstobool (eval map rho n2)))
|Impl (n1,n2) -> Boolans  (imply ((anstobool ( eval map rho n1)),anstobool(eval map rho n2)))
|Equal (n1,n2) -> Boolans ((anstoint(eval map rho n1)) = anstoint(eval map rho n2))
|Gt (n1,n2) -> Boolans    ((anstoint(eval map rho n1)) > anstoint(eval map rho n2))
|Lt (n1,n2) -> Boolans    ((anstoint(eval map rho n1)) < anstoint(eval map rho n2))
|Goe (n1,n2) -> Boolans   ((anstoint(eval map rho n1)) >= anstoint(eval map rho n2))
|Loe (n1,n2) -> Boolans   ((anstoint(eval map rho n1)) <= anstoint(eval map rho n2));;


let rec eval_closure c = match c with Vclosure(t,e) -> eval map t e | Constans(n) -> Constans(n) | Boolans(true) -> Boolans(true) | Boolans(false) -> Boolans(false);;

let rec krivine (c , s) = match (c,s) with
((table,Const n),s) -> Constans n
|((table,Abs n),s) -> abso n
|((table,Identifier n),s) -> eval_closure(map table n)
|((table,Plus(n1,n2)),s) -> Constans ( anstoint (eval_closure(krivine((table,n1),s))) + anstoint (eval_closure(krivine((table,n2),s)) ))
|((table,Sub(n1,n2)),s) -> Constans ( anstoint (eval_closure(krivine((table,n1),s))) - anstoint (eval_closure(krivine((table,n2),s)) ))
|((table, Mult(n1,n2)),s) -> Constans ( anstoint (eval_closure(krivine((table,n1),s))) * anstoint (eval_closure(krivine((table,n2),s)) ))
|((table, Div(n1,n2)),s) -> Constans ( anstoint (eval_closure(krivine((table,n1),s))) / anstoint (eval_closure(krivine((table,n2),s)) ))
|((table, Mod(n1,n2)),s) -> Constans ( anstoint (eval_closure(krivine((table,n1),s))) mod anstoint (eval_closure(krivine((table,n2),s)) ))
|((table, Exp(n1,n2)),s) -> Constans (int_of_float ( ( float_of_int(anstoint (eval_closure(krivine((table,n1),s))) )) ** ( float_of_int(anstoint (eval_closure(krivine((table,n2),s))) ) )))
|((table, T),s) -> Boolans true
|((table, F),s)-> Boolans false
|((table, Not n),s) -> Boolans (not (anstobool (eval_closure(krivine((table,n),s)))))
|((table, And (n1,n2)),s) -> Boolans ((anstobool (eval_closure(krivine((table,n1),s)))) && (anstobool (eval_closure(krivine((table,n2),s)))))
|((table, Or (n1,n2)),s) -> Boolans ((anstobool (eval_closure(krivine((table,n1),s)))) || (anstobool (eval_closure(krivine((table,n2),s)))))
|((table, Impl (n1,n2)),s) -> Boolans (imply (anstobool (eval_closure( krivine((table,n1),s))),anstobool (eval_closure(krivine((table,n2),s)))))
|((table, Equal (n1,n2)),s) -> Boolans (anstoint (eval_closure(krivine((table,n1),s))) = anstoint (eval_closure(krivine((table,n2),s))))
|((table, Gt (n1,n2)),s) -> Boolans (anstoint (eval_closure(krivine((table,n1),s))) > anstoint (eval_closure(krivine((table,n2),s))))
|((table, Lt (n1,n2)),s) -> Boolans (anstoint (eval_closure(krivine((table,n1),s))) < anstoint (eval_closure(krivine((table,n2),s))))
|((table, Goe (n1,n2)),s) -> Boolans (anstoint (eval_closure(krivine((table,n1),s))) >= anstoint (eval_closure(krivine((table,n2),s))))
|((table, Loe (n1,n2) ),s) -> Boolans (anstoint (eval_closure(krivine((table,n1),s))) <= anstoint (eval_closure(krivine((table,n2),s))))
|((table,Lambda(str,e)),Vclosure(table',e')::s) -> krivine(([str,Vclosure(table',e')]@table,e),s)
|((table,Function(e1,e2)),s) -> krivine((table,e1),[Vclosure(table,e2)]@s);;
(* ((table,Lambda(x,e)),Closure(table',e')::s) -> execute_KRIVINE(([(x,Closure(table',e'))]@table,e),s) *)

let e = Function(Lambda("x",Plus(Const(4),Const(5))),Const(4));;

krivine(([],Plus(Const(4),Const(5))),[]);;

krivine(([],e),[]);;

krivine(([],Lambda("x",Identifier("x"))),[Vclosure([],Const(5))]);;

anstoint(krivine((["x",Vclosure([],Const(4))],Identifier("x")),[])) ;;

anstoint(krivine((["x",Vclosure([],Const(4))],Const(5)),[]));;