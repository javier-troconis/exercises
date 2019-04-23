open System.Xml.Xsl
open System.Collections.Generic
open System.Collections
open System
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

//12
type 't thing =
    One of 't
    | Many of int * 't

let rec decode = function
    [] -> []
    | One x::xs -> x::decode xs
    | Many (c,x)::xs -> if c > 2 then x :: decode (Many (c-1,x)::xs) else x::x::decode xs

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;

//16
let drop l n =
    let rec drop c = function
        [] -> []
        | x::xs -> if n > c then x::drop (c+1) xs else drop 1 xs
    drop 1 l

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

let drop_no_backtracking (l:list<'t>) n :seq<'t> =
    let rec drop_no_backtracking (r:Queue<'t>) c = function
        [] -> r
        | x::xs -> 
            if n > c 
            then 
                r.Enqueue(x)
                drop_no_backtracking r (c+1) xs
            else drop_no_backtracking r 1 xs
    drop_no_backtracking (new Queue<'t>()) 1 l :> seq<'t>

drop_no_backtracking ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

//18
let slice list l r = List.skip l list |> List.take (r-l+1)

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

//19
let rec cat = function
    ([], y) -> y
    | (x::xs, y) -> x :: cat (xs, y)

let rec skip = function
    ([], _) -> []
    | (_::xs as xs1, c) -> if c > 0 then skip(xs, c - 1) else xs1

let rec take = function
    ([], _) -> []
    | (x::xs, c) -> if c > 0 then x :: take(xs, c - 1) else []

let cycle l i =
    cat (skip (l, i), take (l, i))
    
take (["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"], 1);;

cycle ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
cycle ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;

//20 
let remove_at n l =
    let rec remove_at c = function
        [] -> []
        | x::xs -> if c = n then xs else x::remove_at (c+1) xs
    remove_at 0 l

remove_at 1 ["a";"b";"c";"d"];;

//21 
let insert_at e i l =
    let rec insert_at c = function
        [] -> [e]
        | x::xs -> 
            if c = i then e::x::xs else x::insert_at (c+1) xs
    insert_at 0 l

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;

//23
//revise
let rand_select l n =
    let get_random_numbers n max =
        let rnd = System.Random()
        let rec get_random_numbers c = if n > c then rnd.Next max :: get_random_numbers (c + 1) else []
        get_random_numbers 0
    let get_nth_element i l =
        let rec get_nth_element c = function
            [] -> raise (System.Exception "not found")
            | x::xs -> if c = i then x else get_nth_element (c + 1) xs
        get_nth_element 0 l
    let rec rand_select = function
        [] -> []
        | x::xs -> get_nth_element x l :: rand_select xs
    rand_select (get_random_numbers n (List.length l))

rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;


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

