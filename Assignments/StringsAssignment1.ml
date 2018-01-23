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



