//1
let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs 

last [ "a" ; "b" ; "c" ; "d" ];;

//2
let rec last_two = function
    | [] -> None
    | [x;x1] -> Some (x,x1)
    | _::xs -> last_two xs 

last_two [ "a" ; "b" ; "c" ; "d" ];;

//3
let at n l =
    let rec at i = function
        | [] -> None
        | x::xs -> if i = n then Some x else at (i+1) xs
    at 1 l

at 3 [ "a" ; "b"; "c"; "d"; "e" ];;

//4
let rec length = function
    | [] -> 0
    | _::xs -> 1 + length xs

length [ "a" ; "b" ; "c"];;

//5 
let rev l =
    let rec rev a = function
        | [] -> a
        | x::xs -> rev (x::a) xs
    rev [] l
    
rev ["a" ; "b" ; "c"];;

//6
let is_palindrome x = x = rev x

is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
not (is_palindrome [ "a" ; "b" ]);;

//7
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten = function
    | [] -> []
    | One x::xs -> x::flatten xs
    | Many x::xs -> flatten x @ flatten xs

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

//8
let rec compress = function
    | x::(x1::_ as xs) -> if x = x1 then compress xs else x :: compress xs
    | x -> x

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

//9
let rec pack = function
    | [] -> []
    | [x] -> [[x]]
    | x::(x1::_ as xs) -> 
        if x = x1 
        then
            let a = pack xs
            let b = List.head a
            let c = x::b
            c::(List.tail a)
        else 
            [x]::pack xs

let pack_1 l =
    let rec pack_1 t r = function
        | [] -> []
        | [x] -> (x::t)::r
        | x::(x1::_ as xs) -> 
            if x = x1 
            then
                pack_1 (x::t) r xs
            else 
                pack_1 [] ((x::t)::r) xs
    List.rev (pack_1 [] [] l)

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;

pack_1 ["a";"a";"e"];;

//10
let rec encode = function
    [] -> []
    | x::xs ->  
        let result = encode xs
        match result with
            (c,x1)::xs1 -> if x = x1 then (c + 1, x)::xs1 else (1, x)::result
            | [] -> [(1, x)] 

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

let encode_no_backtracking l = 
    let rec encode_no_backtracking r c = function
        [] -> r
        | [x] -> (c+1, x) :: r
        | x1::(x2::_ as xs2) -> 
            if x1 = x2 then encode_no_backtracking r (c+1) xs2 else encode_no_backtracking ((c+1, x1)::r) 0 xs2
    encode_no_backtracking [] 0 l |> List.rev



//57
type 't binary_tree =
    Empty
    | Node of ('t * 't binary_tree * 't binary_tree) 

let rec insert n v = 
    match n with
        Empty -> Node (v, Empty, Empty)
        | Node(nv, l, r) -> 
            if nv > v then Node (nv, (insert l v), r) 
            else if v > nv then Node (nv, l, (insert r v)) 
            else n

let construct l = List.fold insert Empty l

construct [3;2;5;7;1]

