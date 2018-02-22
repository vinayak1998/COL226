(*
    Vinayak Rastogi

    2016CS10345

    COL226(Programmign Languages)

    Assignment #3 ==> Terms, Substitutions and Unification

    The problem statement is given below
*)

(*
    Consider the representation of "pre-terms" using the following data type definition

    type term = V of variable | Node of symbol * (term list);;

    Choose suitable type representations for types variable and symbol.



    Given a signature consisting of symbols and their arities (>= 0) in any suitable form --
    either as a list of (symbol, arity) pairs, or as a function from symbols to arities, write a function check_sig that checks
     whether the signature is a valid signature (no repeated symbols, arities are non-negative etc.)

    Given a valid signature (checked using check_sig), define a function wfterm that checks that
    a given preterm is well-formed according to the signature.

    Define functions ht, size and vars that given a well-formed term, return its height (leaves are at height 0) ,
    its size (number of nodes) and the set of variables appearing in it respectively.
    Use map, foldl and other such functions as far as possible wherever you use lists.

    Define a suitable representation for substitutions.

    Come up with an efficient representation of composition of substitutions.

    Define the function subst that given a term t and a substitution s, applies the (Unique Homomorphic Extension of) substitution s
    to t.  Ensure that subst is efficiently implemented.

    Define the function mgu that given two terms t1 and t2, returns their most general unifier,
    if it exists and otherwise raises an exception NOT_UNIFIABLE.

    For each of your programs provide adequate suitable test cases and comment your programs.
*)

(*
    things to make (as done..):
    1) type variable
    2) type symbol
    3) type term
    4) type signature
    5) check_sig function
    6) wfterm function
    7) height function
    8) size function
    9)
*)

open List;;

type variable = string;;
(*
    type variable = string
*)
open Pervasives;;

type symbol = string;;
(*
    type symbol = string
*)

type term = V of variable | Node of symbol * (term list);;
(*
    type term = V of variable | Node of symbol * term list
*)

type arity = int;;
(*
    type arity = int
*)

type signature = (symbol * arity) list;;
(*
    type signature = (symbol * arity) list
*)

let first(e1,e2) = e1;;
(*
    val first : 'a * 'b -> 'a = <fun>

    to return the first element of a pair
*)
let second(e1,e2) = e2;;
(*
    val second : 'a * 'b -> 'b = <fun>

    to return the second element of a pair
*)

let rec check_dup l = match l with
    [] -> true
    | (h::t) ->
       let x = (List.filter (fun x -> x = h) t) in
         if (x == []) then
            check_dup t
         else
       false;;
(*
    val check_dup : 'a list -> bool = <fun>

    this is a function that checks whether the given list
    l has any repeating elements or not

    if it does - return false
    if it does not - return true
*)

check_dup [3;3;4];;
(*
    - : bool = false
*)

let rec buildfirst l = match l with
    [] -> []
|   x::xs -> [(first x)] @ ( buildfirst xs);;
(*
    val buildfirst : ('a * 'b) list -> 'a list = <fun>

    to return a list of first elements of a list of tuples
*)

let rec buildsecond l = match l with
    [] -> []
|   x::xs -> [(second x)] @ ( buildsecond xs);;
(*
    val buildsecond : ('a * 'b) list -> 'b list = <fun>

    to return a list of second elements of a list of tuples
*)

buildfirst [("string",4);("xxx",5)];;
(*
    - : string list = ["string"; "xxx"]
*)

buildsecond [("string",4);("xxx",5)];;
(*
    - : int list = [4; 5]
*)

let rec checkarr l = match l with
    []-> true | x::xs -> (x>0) && (checkarr xs);;
(*
    val checkarr : int list -> bool = <fun>

    to check whether elements of an int array are all positive or not
    if one of them is negative -> returns false
*)

checkarr [-3;-3;4];;
(*
    - : bool = false
*)
# checkarr [4;4;4];;
(*
    - : bool = true
*)

let check_sig l = ( checkarr (buildsecond (l))) && (check_dup (buildfirst (l)));;
(*
    val check_sig : ('a * int) list -> bool = <fun>
*)

let a = [("X" , 4) ; ("X" , 5)];;
(*
    val a : (string * int) list = [("X", 4); ("X", 5)]
*)

check_sig a;;
(*
    - : bool = false
*)

let a = [("X" , 4) ; ("X" , -5)];;
(*
    val a : (string * int) list = [("X", 4); ("X", -5)]
*)

check_sig a;;
(*
    - : bool = false
*)

let a = [("X" , 4) ; ("Y" , -5)];;
(*
    val a : (string * int) list = [("X", 4); ("Y", -5)]
*)

check_sig a;;
(*
    - : bool = false
*)

let a = [("X" , 4) ; ("Y" , 5)];;
(*
    val a : (string * int) list = [("X", 4); ("Y", 5)]
*)

check_sig a;;
(*
    - : bool = true

    the only correct signature test case yet

    hence check_sig works perfectly
*)

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t;;
(*
    val find : 'a -> 'a list -> int = <fun>

    function find returns the index of element x in list l
*)

find 3 [3];;
(*
    - : int = 0
*)

find 4 [5;3;3;3;6;4];;
(*
    - : int = 5
*)

let wfterm signat preterm = match preterm with
| Node (sym,lis) -> (List.length lis) = second (List.nth signat (find sym (buildfirst signat)));;
(*
    Here is an example of a value that is not matched:
    V _

    val wfterm : (symbol * int) list -> term -> bool = <fun>
*)

wfterm [("X",4)] (Node ( "X", [ V "X"]));;
(*
    - : bool = false
*)

wfterm [("X",4)] (Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : bool = true
*)

let rec map f l = match l with
    [] -> []
  |     x::xs -> (f x)::(map f xs);;
(*
    val map : ('a -> 'b) -> 'a list -> 'b list = <fun>

    takes a list l, applies a function f(a unary operation) on each element of l and returns a corrseponding new list
*)

let rec maxlis l = match l with
[] -> 0
|x::xs -> max (x) (maxlis xs);;
(*
    val maxlis : int list -> int = <fun>

    gives the maximum value of an int list
*)

maxlis [3;3;5;4;5;7];;
(*
    - : int = 7
*)

let rec height t = match t with
    V n -> 0
  |     Node(s,l) -> 1 + (maxlis (map height l) );;
(*
    val height : term -> int = <fun>
*)

height (Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : int = 1
*)

let rec sum l = match l with
    [] -> 0
  |     x::xs -> x + (sum xs);;

(*
    val sum : int list -> int = <fun>

    takes in an int list and returns the sum of its elements
*)

sum [3;4;5;6];;
(*
    - : int = 18
*)

let rec size t = match t with
    V n -> 1
  | Node(s,l) -> 1 + sum(map size l);;
(*
    val size : term -> int = <fun>
*)

size (Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : int = 5
*)

let rec listostr n = match n with
[] -> []
|x::xs -> x@listostr(xs);;
(*
    val listostr : 'a list list -> 'a list = <fun>
*)

let rec vars t = match t with
    V n -> [n]
|   Node (s,l) -> listostr (map vars l);;
(*
    val vars : term -> variable list = <fun>
*)

vars (Node ( "Z", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : variable list = ["X"; "q"; "W"; "Y"]
*)

type substitution = (variable * term);;
(*
    type substitution = variable * term

    only to be used for subst
*)

type substitutions = (variable * term) list;;
(*
    type substitution = (variable * term) list

    the main thing, ir the main representation for substitiutions
*)

type compositionsubst = Subs of (variable * term) list | Comp of (substitutions*substitutions);;
(*
    type compositionsubst =
    Subs of (variable * term) list
  | Comp of (substitutions * substitutions)

    Not really used anywhere just an effecient representation

    I have taken care of it in th algorithm itself in the unification part

*)
let rec occurs a b  = match b with
  | V y -> a = y
  | Node (_, l) -> List.exists (occurs a) l;;

(*
    val occurs : variable -> term -> bool = <fun>

    checks if a variable occurs in a term
    important because later on if it does then in some cases the unifier might not exist
*)

occurs "X" (Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : bool = true
*)
occurs "Y"(Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : bool = true
*)
occurs "D" (Node ( "X", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : bool = false
*)
occurs "Z" (Node ( "Z", [ V "X"; V "q"; V "W" ; V "Y"]));;
(*
    - : bool = false
*)

let rec subst s t = match t with
  | V y -> if (first s) = y then (second s) else t
  | Node (f, u) -> Node (f, List.map (subst s) u);;
(*
    val subst : variable * term -> term -> term = <fun>

    here s is substitution that is a pair of (variable,item)
*)

let rec foldr f e l = match l with
    [] -> e
  |     x::xs -> f x (foldr f e xs);;
(*
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun>

    similar to List.fold_right, just a achange of parameters' order

    foldr f b [a1; ...; an]  is f a1 (f a2 (... (f an b) ...)).

    Not tail-recursive...
*)

exception NOT_UNIFIABLE;;
(*
    exception NOT_UNIFIABLE

    an exception that is to be raised when the MGU does not exist
*)


let rec mgu s t= match (s, t) with
    (V x, V y) -> if x = y then [] else [(x, t)]
|   (Node (f, sc), Node (g, tc)) -> if f = g && List.length sc = List.length tc then unify (List.combine sc tc) else raise NOT_UNIFIABLE
|   ((V x, (Node (_, _) as w))
|   ((Node (_, _) as w), V x)) -> if occurs x w then raise NOT_UNIFIABLE else [(x, w)]

and unify s = match s with
    [] -> []
|   (x, y) :: t -> let t2 = unify t in let t1 = mgu (foldr subst x t2) (foldr subst y t2) in t1 @ t2

(*
    val mgu : term -> term -> (variable * term) list = <fun>
    val unify : (term * term) list -> (variable * term) list = <fun>

    takes 2 terms s and t and returns their most general unfier if exists

    if it does not raises exception NOT_UNIFIABLE

    mgu is the main function and unify is the helper function

    "and" used for mutually recursive definition of "unify" and "mgu" where "unify" takes a list of pairs
    and returns the most general unifier and unfiy just takes a pair of terms...
*)