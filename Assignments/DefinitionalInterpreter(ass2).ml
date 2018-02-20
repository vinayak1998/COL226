(*
    Vinayak Rastogi

    2016CS10345

    COL226(Programmign Languages)

    Assignment #2 ==> A definitional interpreter

    The problem statement is given below
*)

(*
    In this assignment, you will define the abstract syntax (data type exp) and a definitional interpreter eval for a simple arithmetic and boolean calculation language.



The expressions in the language are of the following forms

Integer constants,
Unary arithmetic operations: abs, (and any other sensible ones you can think of),
Identifiers, represented as (alphanumeric) strings
binary operations: + (addition), - (subtraction), * (multiplication), div, mod, ^ (exponentiation)
Boolean constants: T and F
Unary boolean operation: not
binary boolean operations:  /\ (and), \/ (or), -> (implies)
Comparison operators: = (equal) , > (greater than), < (less than) , >= (greater or equal), <= (less or equal) on integer expressions
n-tuples for each n > 2
Projection operators proj(i,n) which project the ith element of an n-tuple.


Assume all inputs are of proper type (we will study type-checking later).  Define a suitable data type answer.



eval: exp -> answer.

Next, define a suitable set of opcodes for a stack-and-table machine to evaluate this language and define a compiler for this language to sequences of these opcodes.

compile: exp -> opcode list

Third, define the stack machine execution functions, which takes a sequence of opcodes and executes them starting from a given stack and table.

execute: stack * table * opcode list -> answer

Provide enough examples
*)

open List;;
(*
    open list feature
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
  | Constructor of exp list
  | Proj of int * exp list;;

(*
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
  | Constructor of exp list
  | Proj of int * exp list


  This is a data type (abstract syntax) exp
*)

type answer = Constans of int|Boolans of bool|Tuplans of answer list ;;
(*
    type answer = Constans of int | Boolans of bool | Tuplans of answer list

    This is the suitable data type answer
*)

let abso e = match e with Const(n) -> Constans(abs n);;
(*
    val abso : exp -> answer = <fun>

    this is to convert absolute of an int value to abswer type given a exp type
*)

let anstoint t = match t with
Constans n -> n;;
(*
    val anstoint : answer -> int = <fun>

    This is to convert an answer type to int type
*)

let anstobool t = match t with
Boolans n -> n;;
(*
    val anstobool : answer -> bool = <fun>

    This is to convert an answer type to bool type
*)

let rho t = match t with
"x" -> Constans 10
| _ -> Constans 0;;
(*
    val rho : string -> answer = <fun>

    arbitrary rho function to map variables
*)


let imply e = match e with
(true,false) -> false
| _ -> true;;
(*
    val imply : bool * bool -> bool = <fun>

    Logical Implies function
    false only when true->false
*)

let rec eval rho t = match t with
Const n -> Constans n
|Abs n -> abso n
|Identifier n -> rho n
|Plus(n1,n2) -> Constans ((anstoint (eval rho n1)) + (anstoint (eval rho n2)))
|Sub(n1,n2) -> Constans ((anstoint (eval rho n1)) - (anstoint (eval rho n2)))
|Mult(n1,n2) -> Constans ((anstoint (eval rho n1)) * (anstoint (eval rho n2)))
|Div(n1,n2) -> Constans ((anstoint (eval rho n1)) / (anstoint (eval rho n2)))
|Mod(n1,n2) -> Constans ((anstoint (eval rho n1)) mod (anstoint (eval rho n2)))
|Exp(n1,n2) -> Constans (int_of_float ( ( float_of_int(anstoint (eval rho n1)) ) ** ( float_of_int(anstoint (eval rho n2)) ) ))
|T -> Boolans true
|F -> Boolans false
|Not n -> Boolans (not (anstobool(eval rho n)))
|And (n1,n2) -> Boolans ((anstobool (eval rho n1)) && (anstobool (eval rho n2)))
|Or (n1,n2) -> Boolans ((anstobool (eval rho n1)) || (anstobool (eval rho n2)))
|Impl (n1,n2) -> Boolans (imply (anstobool( eval rho n1),anstobool(eval rho n2)))
|Equal (n1,n2) -> Boolans (anstoint(eval rho n1) = anstoint(eval rho n2))
|Gt (n1,n2) -> Boolans (anstoint(eval rho n1) > anstoint(eval rho n2))
|Lt (n1,n2) -> Boolans (anstoint(eval rho n1) < anstoint(eval rho n2))
|Goe (n1,n2) -> Boolans (anstoint(eval rho n1) >= anstoint(eval rho n2))
|Loe (n1,n2) -> Boolans (anstoint(eval rho n1) <= anstoint(eval rho n2));;
(*
    val eval : (string -> answer) -> exp -> answer = <fun>

    eval rho function w/o handling n tuples, for mainly using it to convert exp list to ans list
*)

let rec explitoansli n = match n with
(x::xs) -> [eval rho x] @ explitoansli xs
| [] -> [];;
(*
    val explitoansli : exp list -> answer list = <fun>

    for Converting exp list to ans list using the above partially defined eval
*)

let exptoans t = match t with
Const n -> Constans n
|T -> Boolans true
|F -> Boolans false;;
(*
    val exptoans : exp -> answer = <fun>

    Exp to ans for the projection part
*)

let rec eval rho t = match t with
Const n -> Constans n
|Abs n -> abso n
|Identifier n -> rho n
|Plus(n1,n2) -> Constans ((anstoint (eval rho n1)) + (anstoint (eval rho n2)))
|Sub(n1,n2) -> Constans ((anstoint (eval rho n1)) - (anstoint (eval rho n2)))
|Mult(n1,n2) -> Constans ((anstoint (eval rho n1)) * (anstoint (eval rho n2)))
|Div(n1,n2) -> Constans ((anstoint (eval rho n1)) / (anstoint (eval rho n2)))
|Mod(n1,n2) -> Constans ((anstoint (eval rho n1)) mod (anstoint (eval rho n2)))
|Exp(n1,n2) -> Constans (int_of_float ( ( float_of_int(anstoint (eval rho n1)) ) ** ( float_of_int(anstoint (eval rho n2)) ) ))
|T -> Boolans true
|F -> Boolans false
|Not n -> Boolans (not (anstobool(eval rho n)))
|And (n1,n2) -> Boolans ((anstobool (eval rho n1)) && (anstobool (eval rho n2)))
|Or (n1,n2) -> Boolans ((anstobool (eval rho n1)) || (anstobool (eval rho n2)))
|Impl (n1,n2) -> Boolans (imply (anstobool( eval rho n1),anstobool(eval rho n2)))
|Equal (n1,n2) -> Boolans (anstoint(eval rho n1) = anstoint(eval rho n2))
|Gt (n1,n2) -> Boolans (anstoint(eval rho n1) > anstoint(eval rho n2))
|Lt (n1,n2) -> Boolans (anstoint(eval rho n1) < anstoint(eval rho n2))
|Goe (n1,n2) -> Boolans (anstoint(eval rho n1) >= anstoint(eval rho n2))
|Loe (n1,n2) -> Boolans (anstoint(eval rho n1) <= anstoint(eval rho n2))
|Constructor n -> Tuplans (explitoansli n)
|Proj (i,n) -> exptoans (List.nth n i);;
(*
    val eval : (string -> answer) -> exp -> answer = <fun>

    eval function giving out answer type
*)

type opcode =
    CONST of int
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
  | CONSTRUCTOR of (opcode list) list
  | PROJ;;

(*
    type opcode =
    CONST of int
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
  | CONSTRUCTOR
  | PROJ

  This is the OPCODE type
*)

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
|Loe (n1,n2) -> (compile n1)@(compile n2)@[LOE];;
(*
    val compile : exp -> opcode list = <fun>

    Partial compile fucntion
*)

let rec explitoopcli n = match n with
(x::xs) -> (compile x) @ explitoopcli xs
| [] -> [];;
(*
    val explitoopcli : exp list -> opcode list = <fun>

    this converts exp list to opcode list
*)

(*
    Now I've written the compile function to convert exp to a sequence of opcode lists to be executed

    exp -> opcode list
*)
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
|Constructor n -> [CONSTRUCTOR [explitoopcli n]]
|Proj (i,n) -> [CONST i]@(compile (Constructor n))@[PROJ];;
(*
    val compile : exp -> opcode list = <fun>
*)

let table t = match t with
"x" -> Constans 10
| _ -> Constans 0;;
(*
    val table : string -> answer = <fun>
*)

let absoexe e = match e with Constans(n) -> Constans(abs n);;
(*
    val absoexe : answer -> answer = <fun>
*)

let rec execute (s,table,c) = match (s,table,c) with
(s,table,[]) -> hd s
|(s,table,CONST(n)::c') -> execute(Constans(n)::s , table, c')
|(n1::n2::s,table,PLUS::c') -> execute(Constans ((anstoint n1) + (anstoint n2))::s , table ,c')
|(n1::n2::s,table,SUB::c') -> execute(Constans ((anstoint n2) - (anstoint n1))::s, table ,c')
|(n1::n2::s,table,MULT::c') -> execute(Constans ((anstoint n1) * (anstoint n2))::s, table,c')
|(n1::n2::s,table,DIV::c') -> execute(Constans ((anstoint n2) / (anstoint n1))::s,table,c')
|(n1::n2::s,table,MOD::c') -> execute(Constans ((anstoint n2) mod (anstoint n1))::s,table,c')
|(n1::n2::s,table,EXP::c') -> execute(Constans (int_of_float ( ( float_of_int(anstoint n2) ) ** ( float_of_int(anstoint n1) ) ))::s,table,c')
|(n::s,table,ABS::c') -> execute(absoexe(n)::s, table, c')
|(s, table, IDENTIFIER(n)::c' ) -> execute(table(n)::s, table, c')
|(s, table, TRUE::c') -> execute(Boolans true::s, table, c' )
|(s, table, FALSE::c') -> execute(Boolans false::s, table, c' )
|(n::s , table , NOT::c') -> execute( Boolans (not (anstobool n)) ::s,table, c')
|(n1::n2::s, table, AND::c') -> execute(Boolans ((anstobool n2) && (anstobool n1))::s, table, c')
|(n1::n2::s, table, OR::c') -> execute(Boolans ((anstobool n2) || (anstobool n1))::s, table, c')
|(n1::n2::s, table, IMPL::c') -> execute(Boolans (imply (anstobool n2,anstobool n1))::s, table, c')
|(n1::n2::s, table, EQUAL::c') -> execute(Boolans (anstoint n2 = anstoint n1)::s, table, c')
|(n1::n2::s, table, GT::c') -> execute(Boolans (anstoint n2 > anstoint n1)::s, table, c')
|(n1::n2::s, table, LT::c') -> execute(Boolans (anstoint n2 < anstoint n1)::s, table, c')
|(n1::n2::s, table, GOE::c') -> execute(Boolans (anstoint n2 >= anstoint n1)::s, table, c')
|(n1::n2::s, table, LOE::c') -> execute(Boolans (anstoint n2 <= anstoint n1)::s, table, c')
|(s, table, CONSTRUCTOR (x::xs)::c') -> execute(Tuplans([execute ([],table,x)]@[execute ([],table,[CONSTRUCTOR(xs)])])::s,table,c')
|(Constans(i)::Tuplans(n)::s, table, PROJ::c') -> execute((List.nth n i)::s,table, c');;
(*
    let rec opclilitoansli (s:answer list) (table:string->answer) (c:opcode list) : (answer) = match c with
| (CONSTRUCTOR (x::xs))::something -> [execute (s,table,x)]@[execute (s,table,CONSTRUCTOR([xs]))];;
*)



let a = Plus(Const 1, Const 1);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Sub(Const 1, Const 1);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Mult(Const 22, Const 243);;
eval rho a;;
execute ([], rho, (compile a));;

let a = And(F, T);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Or(F, T);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Not(T);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Impl(F, T);;
eval rho a;;
execute ([], rho, (compile a));;

let a = Equal(Const 7, Sub(Const 10, Const 3));;
eval rho a;;
execute ([], rho, (compile a));;

let a = Proj(0, [Const 1]);;
eval rho a;;
execute ([], rho, (compile a));;



