open System.ComponentModel
open System.Runtime.InteropServices

let f_2_1 n = n % 2 = 0 || n % 3 = 0 && n % 5 <> 0

let f_2_2 (x, y) = 
    let (+.) x y : string = x + y
    let rec f_2_2' = function
        | (y, 0) -> y
        | (y, z) -> f_2_2' (y +. x, z - 1)
    f_2_2' ("", y)

let f_2_3 (str : string, i, ch) = str.[i] = ch

//2.4
let occFromIth (str : string, i, ch) = 
    if (i >= String.length str) then 0
    else
        Seq.skip i str
        |> Seq.filter (fun x -> x = ch)
        |> Seq.length

//2.5
let occInString (str, ch) = 
    str
    |> Seq.filter (fun x -> x = ch)
    |> Seq.length

//2.6
let notDivisibleBy (d, n) = n % d <> 0

//2.7.1
let test a b c = 
    b >= a 
    && notDivisibleBy (a, c) 
    && notDivisibleBy (a + 1, c) 
    && notDivisibleBy (b, c) 

//2.7.2
let prime x =
    let rec prime y =
        let z = x % y
        x > 1 && z = 0 && x = y || prime (y + 1)
    prime 2
    
prime 2


List.filter prime [1]

let rec gcd = 
    function 
    | (x, 0) -> x
    | (x, y) -> gcd (y, x % y)

type tree<'c> = 
    | Empty
    | Node of 'c tree * 'c * 'c tree

type k<'a, 'b> = 
    | A of 'b
    | B of 'a

type list'<'t> = 
    | Head of 't
    | Tail of 't list'

let rec create_list t = 
    match t with
    | Empty -> []
    | Node(a, b, c) -> create_list a @ [ b ] @ create_list c

let make_list n = 
    let rec f n a = 
        match n with
        | 0 -> a
        | _ -> f (n - 1) (n :: a)
    f n []

let reverse_tail l = 
    let rec f l a = 
        match l with
        | [] -> a
        | h :: t -> f t (h :: a)
    f l []

let reverse_tail1 l = 
    let rec f = 
        function 
        | ([], a) -> a
        | (h :: t, a) -> f (t, h :: a)
    f (l, [])



let rec reverse = function 
    | [] -> []
    | h::t -> reverse t @ [h]

let rec take = function
    | [] -> []
    | h::t -> h::skip(t)
    and
    skip = function
    | [] -> []
    | _::t -> take t


let rec max = function
    | [x] -> x
    | h::t -> 
        let h1 = max t
        if h > h1 then h else h1

let rec merge = function
    | (x, []) -> x
    | ([], x) -> x
    | ((x::xs as a), (y::ys as b)) -> 
        if x > y then y::merge(a, ys) else x::merge(xs, b)

let rec sum_lists = function
    | [] -> 0
    | []::xs -> sum_lists(xs)
    | (x::xs)::y -> x + sum_lists(xs::y)



let rec alt = function
    | [] -> ([],[])
    | x::xs -> 
        let (a,b) = alt(xs)
        if List.length xs % 2 = 0 then (x::a,b) else (a,x::b)

let rec alt1 = function
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x1::x2::xs -> 
        let (a,b) = alt1(xs)
        (x1::a,x2::b)

let rec sum_cps l f =
    match l with
    | [] -> f 0
    | x::xs -> sum_cps xs (fun y -> f (x + y))


let testA = (+) 1
let testB = (+)
let testC x = (+) x
let testD x = x 1 + 1
let testE x y z = x + y + z
let testF x = (+) (x 1)
let testG x y = y (x + 1) + 1
let testH x = x 1 1 + 1

let sum_if_true test a b =
    (if test a then a else 0) + (if test b then b else 0)

let rec gdc (a, b) = 
    match a % b with
    0 -> b
    | c -> gdc (b, c)



 









        
       
