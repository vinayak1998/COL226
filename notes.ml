# 1+1;;
- : int = 2

# let average a b =
  (a +. b) /. 2.0;;
  val average : float -> float -> float = <fun>

# true;;
- : bool = true

(*
    0 = {}
    1 = {()}

    2^0 = 1 = {0}

    1 X 1 ~= 1
    1 X 0 = 0 X 1 = 0
*)

# let iso x = match x with
    T -> true
      | F -> false;;
Error: Unbound constructor T
(*
    1) "match" is used for case analysis
    2) T -> true | F -> false are Constructors
    3) this is an isomorphism
    4) a mapping function
*)

(*
    A --> B == B^A
    2 --> 2 == 2^2 = 4

    A + 0 = A = 0 + A
*)

# type nat = O | S of nat;;
type nat = O | S of nat
(*
    type <name> = x1 | x2 | x3 | ... | xn
                 |_______________________|
                       enumeration
    nat is set with O as an element of Nat.
    nat is also closed under constructor S, ie S of .... is an element of nat
    nat => countably infinite
    no meaning defined yet though
*)

(*
    constructors -> confirm/follow certain rules
                    no meaning necessary
*)

# let swap (a,b) = (b,a);;
val swap : 'a * 'b -> 'b * 'a = <fun>
(*
    AXB and BXA are isomorphic
*)

# O(O);;
Error: The constructor O expects 0 argument(s),
       but is applied here to 1 argument(s)

# S(O,O);;
Error: This expression has type 'a * 'b
       but an expression was expected of type nat
(*
    nat type expected in brackets
*)

# S(O);;
- : nat = S O

(*
    S and O can't be used arbitrarily

    O => element of nat
    S => Constructor, takes a nat, returns a nat
*)

# let rec count x = match x with
    O -> 0
      | S y -> 1 + (count y);;
val count : nat -> int = <fun>
(*
    let is used to define functions
    rec is used for recursive definitions
    here we defined a function by cases
    count = {
        0 if x=O
        1 + (count y) if x = S y
    }
    input x is going to be of type nat, output will be of type int
    thus, nat -> int
*)

(*
    SYNTAX : nat == O, S O, S (S O), S (S (S O)).......
             N   == 0, 1, 2, 3.....

*)

# let succ x = S x;;
val succ : nat -> nat = <fun>
(*
    nat -> nat
    one one function
    successor function
*)

# succ O;;
- : nat = S O
# succ (S O);;
- : nat = S (S O)
# succ (S (S O));;
- : nat = S (S (S O))

# let crazy x = match x with
    O -> 5
    | S y -> 7;;
val crazy : nat -> int = <fun>
(*
    crazy == f:nat -> int
    proof technique about anything from nat => induction

    any function defined on the basis of nat => recurive function
*)

# let rec double x = match x with
        O -> O
  |     S y -> S (S double y);;
Error: Syntax error: operator expected.

# let rec double x = match x with
    O -> O
  |     S y -> S (S ( double y));;
val double : nat -> nat = <fun>
(*
    f: nat -> nat
    k knots,
    for each know divide it into 2 knots,
    and then do that recursively
*)

# let compose f g x = g (f x);;
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = <fun>
(*
    arguments are functions here
    g ----> f, f ----> x, ===> g ----> x is what's being done
*)

# let doublecount = compose double count;;
val doublecount : nat -> int = <fun>
(*
    composes double(count)
    doublecount: nat ---> int
    double: nat ---> nat
    count: nat ---> int
*)

# let curry f a b = f (a, b);;
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
(*
            |_______|
          input to func.
    curry returns a func. that takes arguments from type of 1 component of arg,
    and returns the value taken from the type of 2nd component of arg

    f: A X B --> C
    curry f: A ---> [B ---> C]

    LHS: 'a->('b->('c->'d

        f: 'a
        a: 'b
        b: 'c
        : means (belongs to, set membership, not equality)

    RHS: f applied to a pair ie. (a,b)
                                |_____|
                         must be of type ('b * 'c)

    'a = ('b * 'c) -> 'd
    |___________________|
              |________________curry figured this constraint from usage of
                               f with a,b : equational solving


    f-> a-> b-> c()let's say some output
    to usne ye figure out kiya ki f aage jaake (a,b) ko input le raha hai hence
    f is of type 'a*'b -> 'c

    therefore
    f-> a-> b-> c becomes ('a * 'b -> 'c) -> 'a -> 'b -> 'c
*)

(*
    type == right associative
    let == left associative
*)

# let uncurry g (a, b) = (g a) b;;
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun>
(*
    f: A ---> [B ---> C]
    curry f: A X B --> C

    takes 2 arguments, 2nd argument is a pair

    uncurry: 'c -> ('0 -> 'd)

    g: 'c

    (a,b) : '0
             |__'a*'b

             a: 'a
             b: 'b

    'c = 'a -> 'm

    same concept apply karke dekh curry wala
*)

(*
    An Inductive Construction:

    give meaning to something, eg: func. count above

    given any set A, I can build list of elements drawn from A
                                 |___|
                                   |_______built in oCaml
*)

# open List;;
# [];;
- : 'a list = []
(*
    empty list
*)
# [3];;
- : int list = [3]
(*
    singleton
    figures out itslef it's an int list
*)
# [true];;
- : bool list = [true]

# [3,4,5];;
- : (int * int * int) list = [(3, 4, 5)]
# [3; 4; 5];;
- : int list = [3; 4; 5]

# [3,true];;
- : (int * bool) list = [(3, true)]
# [3;true];;
Error: This expression has type bool but an expression was expected of type
         int

(*
    base case: empty list
    inductive case: 1st element -> 'a (TYPE)
           rest of the elements -> 'a list
*)

# hd[3;4;5];;
- : int = 3
# tl [3;4;5];;
- : int list = [4; 5]

# let rec length l = match l with
    []->0
  | x::xs->1+ (length xs);;
val length : 'a list -> int = <fun>
(*
    x is the head
    :: is the separator
    xs is the tail
*)

# length[3;4;5];;
- : int = 3

(*
    [] => constructor
    |___ nil
    |___ empty list

    (a single element) :: (a list)

    1+(1+(1+0)) === 3

    computation as we pop off elements from stack

    time, space ~~ O(n)
*)

# let rec length' l m = match l with
    [] -> m       (* empty list*)
  | (x::xs) -> length' xs (1+m);;
val length' : 'a list -> int -> int = <fun>
(*
    f: 'a list -> int -> int
    length prime is tail recursive hence computationally better than length
*)

# let length l = length' l 0;;
val length : 'a list -> int = <fun>
# length [3;4;5];;
- : int = 3

# let rec sum l = match l with
    [] -> 0
  |     x::xs -> x + (sum xs);;
val sum : int list -> int = <fun>
(*
    because of 0, we can see that the return type is int
    xs -> int list
    x , sum xs -> int
*)

# let rec sum' l e = match l with
    [] -> e
  | x::xs -> sum' xs (x+e);;
val sum' : int list -> int -> int = <fun>
(*
    more space effecient
    same as sum l for "e = 0"
    tail recursive
*)

(*
    Coersion => int(+) ---------------------> float(real #)(+.)
                        convert ghost func.

*)

# let rec prod l = match l with
    [] -> 1
  | x::xs -> x * (prod xs);;
val prod : int list -> int = <fun>

# prod [3;4;5];;
- : int = 60

# let rec foldr f e l = match l with
    [] -> e
  |     x::xs -> f x (foldr f e xs);;
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = <fun>
(*
    lifting off a binary operator to a k-ary operation

    O(n) in time and space

    operates f(any binary operation) of the element e with the last element of list l, and then that to the second last
    element, and then their result to the 3rd last element and so on..

    eg: is f is sum, it will add all the elemets of the list and then add e to the sum

    CAN also be written tail recursively
*)

# let rec foldl f e l = match l with
    [] -> e
  | x::xs -> foldl f (f (x, e)) xs;;
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = <fun>
(*
    tail recursive version of foldr

    har f (f (x, e)) xs ke call pe xs ko exploit karta jayega
    and aage wale element se computation chalu kardega f wala
*)

# let rec map f l = match l with
    [] -> []
  |     x::xs -> (f x)::(map f xs);;
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
(*
    map reduce

    ek list ke har element pe f(unary operation) lagata hai and nyi list deta hai
*)

# type 'a bintree = Tip | Node of 'a * 'a bintree * 'a bintree;;
type 'a bintree = Tip | Node of 'a * 'a bintree * 'a bintree
(*
    TIP -> Base case -> empty tree
*)

(*
    Constructors in oCaml : always start with => Capitals (exceptions: true, false)

    names of func, var., types => small letters

    notion of equality of trees => have to be EXACTLY the same

    constructor tree => Tip -. has no arguments ,tell us it's an empty tree

    Node => Constructor with left and right subtree

    Binary trees: 1) empty as base case
                  2) given root node + 2 subtrees

    bintree => constructor of Binary Trees
               parameterized on type 'a : arbitrary(can be whatever I want: int, float, bool...)
*)

# exception EmptyTree;;
exception EmptyTree
(*
    Exception -> EmptyTree (to be raised when an operation works only on non empty trees)

    defined the exception here
*)

# let left t = match t with
        Tip -> raise EmptyTree
    |   Node (x, l, r) -> l;;
val left : 'a bintree -> 'a bintree = <fun>
(*
    Tip: Empty Tree
    Node(x, l, r) : Non-Empty Tree
    l: left subtree of the root returned
*)

(*
    OCaml is sequential in the order of looking at cases

    FOR DEFAULT??

    :: -> Cons

    @  -> Append
*)

# max;;
- : 'a -> 'a -> 'a = <fun>

# let rec height t = match t with
    Tip -> 0
  |     Node(x, l, r) -> 1 + (max(height l)(height r));;
val height : 'a bintree -> int = <fun>
(*
    max is curry :D

    f: 'a bintree -> int

    tells us height of the tree
*)

# let rec size t = match t with
    Tip -> 0
  | Node(x, l, r) -> 1 + (size l) + (size r);;
val size : 'a bintree -> int = <fun>
(*
    f: 'a bintree -> int

    tells us size of the tree, ie. , the number of nodes
*)

(*
    use induction on structure of tree

    using height/size (height preferably) for proofs
*)

# let rec posttrav t = match t with
    Tip -> []
  | Node(x, l, r) -> (posttrav l) @ (posttrav r) @ [x];;
val posttrav : 'a bintree -> 'a list = <fun>

(*
    f: 'a bintree -> 'a list

    @ => only works for lists on both sides

    THIS IS POSTORDER TRAVERSAL

            3
         ___|___
        |       |
        4       5
      __|__    _|___
    Tip   Tip |     |
              7     Tip
            __|__
          Tip   Tip

    Postorder: 4, 7, 5, 3
*)

# let rec pretrav t = match t with
    Tip -> []
  | Node(x, l, r) -> [x] @ (pretrav l) @ (pretrav r);;
val pretrav : 'a bintree -> 'a list = <fun>

(*
    Preorder traversal

    3,4,5,7
*)

# let rec inotrav t = match t with
    Tip -> []
  | Node(x, l, r) -> (inotrav l) @ [x] @ (inotrav r);;
val inotrav : 'a bintree -> 'a list = <fun>
(*
    4,3,7,5
*)

(*
    QUESTION: Prove that length of posttraversal of tree = size of tree

    use height for induction
    prove |l1 @ l2| = |l1 + l2|

    let | posttraversal l| = m
    let | posttraversal r| = n
    |[x]|=1
*)

(*
    abstract syntax (think about trees always) -> tiny sublanguage

    define a language of expression
*)

# type exp = Const of int | Plus of exp*exp;;
type exp = Const of int | Plus of exp * exp
(*
    eg:
    Const(3)
    or

            Plus
            /  \
          Plus  Const(9)
          /  \
    Const(4) Const(7)
*)

# let x = Plus(Const(4),Const(7));;
val x : exp = Plus (Const 4, Const 7)
# let y = Plus(Const(7),Const(4));;
val y : exp = Plus (Const 7, Const 4)
# x=y;;
- : bool = false
(*
    Because as of now, Plus has no meaning and is only a constructor
*)

# let rec eval e = match e with
    Const n -> n
  | Plus(e1,e2) -> (eval e1) + (eval e2);;
val eval : exp -> int = <fun>
(*
    now this function gives MEANING to each subtree

    a definitional interpretor

    total function operating on trees of type exp

    takes in a tree of type exp ------(returns)-----> int value

    eval e ={ n     if e is Const(n)
                    if e is +(Plus)
                            /    \
                            e1   e2
            }

    e1 and e2 are subexpressions, references of god's binary operation (inorder traversal)

    e goes and recursively finds value of e1 and e2, gets their meaning and values, and adds them up


*)

# eval x = eval y;;
- : bool = true

(*
    Question: Define height and size on expression
    Use induction to prove for any 2 trees e1 and e2,
    Plus(e1,e2) = e1 + e2
    (hin:- try induction on height)
*)

# Plus;;
Error: The constructor Plus expects 2 argument(s),
       but is applied here to 0 argument(s)

# type opcode = CONST of int | PLUS;;
type opcode = CONST of int | PLUS
(*
    Here, the "PLUS" is not going to take any argument
    it is a 0-ary constructor, a constant symbol
*)

# PLUS;;
- : opcode = PLUS

# let rec compile e = match e with
    Const n -> [CONST(n)]
  | Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS];;
val compile : exp -> opcode list = <fun>
(*
    Const n is like Tip here
    Plus is an Operation
    [CONST(n)] is a singleton list with a single opcode
    compile e1: left subexpression
    compile e2: right subexpression

    this is very similar to eval function

    Here we're compiling l1,l2 , appending the list obtained on compiling l2 to list obtained on compiling l1
    and appending [PLUS] to them

    f: exp -> opcode list
*)

# let rec execute (s,c) = match (s,c) with
        (s,[]) -> hd s
  | (s, CONST(n)::c') -> execute(n::s,c')
  | (n2::n1::s',PLUS::c') -> execute(n1+n2::s',c')
  | _ -> raise EmptyTree;;
val execute : int list * opcode list -> int = <fun>
(*
    |   |
    |~~~|   [] => halt and do something
    |   |
    |___|
      s

    |   |                        | n |
    |~~~|  Const(n)::c'          |~~~| , c'
    |   |  (doesn't care what's  |   |
    |___|  in the stack already) |___|
      s                            s

      n2
    | n1|   PLUS::c'                           ===>      | n3|
    |~~~|   (expects something on the stack)             |~~~| , c'   where n3 = n2+n1
    |   |                                                |   |
    |___|                                                |___|
      s                                                    s
*)

# let z = Plus(x,y);;
val z : exp = Plus (Plus (Const 4, Const 7), Plus (Const 7, Const 4))
# let prgm = compile z;;
val prgm : opcode list =
  [CONST 4; CONST 7; PLUS; CONST 7; CONST 4; PLUS; PLUS]
# execute ([],prgm);;
- : int = 22
# eval z;;
- : int = 22
(*
    look at handwritten notes for reference
*)

(*
    compiler -> want to run it on a particular kind of machine
    prgm ∈ opcode list

    eval -> interpreter
    e ∈ exp -> programming language

    e ∈ exp -----eval(1)(interpreter)--------> n ∈ Z(RESULT)
    e ∈ exp -----compile(2)------> (prgm ∈ opcode list) == (s ,prgm) ----completed the picture(3)----> n ∈ Z(RESULT)

    (s, prgm) -> the assembly level obtained
    (3) => runnig assembly level program on a machine

    SECURITY <= compile,execute -> if a program given is the result of compiling an expression
                                   and I started w an arbit stack, I'd not look at what the contents
                                   are, nor violate the integrity of the stack

    (COMPILING + RUNNING)   gives the same result as (INTERPRETER)

    definitional interpretor => The meaning of the whole is obtained from the mening of the parts

                                There is a recursive and deterministic function to evaluate a complex expression
                                by evaluating sub-expressions and then using the results

    Conceptually, stack is unbound

    |   |
    |~~~|  => when do you look below the sea level?
    |   |
    |___|
      s

    stack smashing (in C)? => large string arguments go beneath the sea level
                              when return hit, not only the vaue is returned, but the address as well
                              and then the address is modified and taken to the address of the virus
                              installed to be executed

    @ least scream, whenever the algorithm tries to cross the laxman rekha

    moral :- keep the stack abstractions vv secure

    read => Meltdown!

    to make the system faster, the processor looks into the parts of the memory where it should not

    sandbox abstraction was broken down


*)










