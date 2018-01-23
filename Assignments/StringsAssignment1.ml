(*
    Vinayak Rastogi

    2016CS10345

    COL226
*)

(*
    Consider the data type A* of strings over a given alphabet A, which inductively defined as:

    The empty string (which has no letters) is in A*
    Each letter a in A is in A*
    If s_1 and s_2 are strings in A*, then their concatenation s_1 s_2 is in A*

Define in OCaml the following:

    An efficient functional data type to represent editable strings (which can be arbitrarily long) over an arbitrary given alphabet.

    A function lgh, which given a string over the given alphabet returns a non-negative integer which is the number of letters in the string.

    A function nonempty that returns false if a given string is empty and true otherwise.

    A function concat that concatenates two given strings  [ What is its complexity?
    Also prove that lgh( concat s1 s2) = lgh(s1) + lgh(s2). ]

    A function reverse, which reverses the characters in a string   [ Its complexity should be O(lgh(s)).
    Also prove that  lgh(reverse s) = lgh(s). ]

    A function first that returns the first letter of a given non-empty string, raising an exception Empty otherwise.
    [ This should be O(1). ]

    A function last that returns the last letter of a given non-empty string, raising an exception Empty otherwise.
    [ This should be O(1). ]

{Edit functions, assuming that when creating a string,  the initial position of the marker will be 0. ]

    A function create which given an OCaml string, creates an editable string, with the initial position of the edit marker being 0

    A function forward that when a marker points to the kth position in the string moves it to the (k+1)-th position, if it exists,
    and raising AtLast otherwise.
    [ What is its complexity? We want it to be O(1).  By default, the initial position of the marker when creating a string  is 0. ]

    A function back that when the marker points to the kth position in the string moves it to the (k-1)-th position, if it exists,
    and raising AtFirst otherwise. [ What is its complexity? We want it to be O(1). ]

    A function moveTo that given a non-negative integer n and a string s, moves the marker to the nth letter of s, counting from 0.
    If n >= lgh s, then raise exception TooShort.  [ What is its complexity? We want it to be O(n), where n is the given argument. ]

    A function replace which (assuming the marker is at a position n>= 0) in a string s, and a letter w,
    replaces the letter at the n-th position of s with w.  [ Prove that lgh(replace w s) = lgh(s). ]
*)

(*
type 'a str = Empty | Letter of 'a | Concat of ('a str)*('a str);;
type 'a str = Empty | Letter of 'a | Concat of 'a str * 'a str

let rec lgh e = match e with
    Empty -> 0
  | Letter n -> 1
  | Concat(e1,e2) -> (lgh e1) + (lgh e2);;

let nonempty e = match e with
    Empty ->  false
  | _ -> true;;

let concat e = match e with
    (e1,e2) -> e1^e2;;

let reverse s =
    let rec helper i =
        if i >= String.length s then "" else (helper (i+1))^(String.make 1 s.[i])
    in
        helper 0;;

exception EmptyString;;

let first e = match e with
    "" -> raise EmptyString
  | _ -> String.get e 0;;

let last e = match e with
    "" -> raise EmptyString
  | _ -> String.get e ((String.length e)-1);;


__________________________________________________________________________________________

*)

exception AtLast;;
exception AtFirst;;
exception Empty;;
exception TooShort;;


type 'a str =  String of 'a list;;

let rec lgh l =  match l with
  String [] -> 0
  | String (x::xs) -> 1 + (lgh (String xs));;

let nonempty l =
    if (lgh l)  !=0 then true
    else false;;

let concat (a,b) = match (a,b) with
    (String e1, String e2) ->  String (e1 @ e2);;

let x = String [1;2;3];;

let y = String [4;5;6];;

let z = concat(x,y);;

lgh z = (lgh x) + (lgh y);;

let tolist l = match l with
    String n -> n;;

let tocustom l = match l with
    n -> String n;;

z = tocustom (tolist z);;

let reverse l = tocustom ( List.rev ( tolist l));;

lgh x = lgh (reverse x);;

open List;;

let first l = match l with
    String [] -> raise Empty
  | String n ->    hd n;;

let last l = match l with
    String [] -> raise Empty
  | String n -> List.nth n ((List.length n)-1);;

let stol s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let create l = (String (stol l) , 0);;

let returnmarker l = match l with
    (a,b) -> b;;

let dt l = match l with
    (a,b) -> a;;

let forward l = if lgh(dt l)-1=returnmarker l then raise AtLast else (dt l,(returnmarker l)+1);;

let backward l = if 0=returnmarker l then raise AtFirst else (dt l,(returnmarker l)-1);;

let moveTo n l = if n >= lgh (dt l) then raise TooShort else (dt l, n );;

let rec replaceelem ls x elem count=
  match ls with
  | [] -> ls
  | h::t -> if (count = x) then
          elem::(replaceelem t x elem (count+1))
        else
          h::(replaceelem t x elem (count+1));;

let replaceindex ls x elem = replaceelem ls x elem 0;;

let replace l w = (String (replaceindex (tolist (dt l)) (returnmarker l) w) ,returnmarker l);;