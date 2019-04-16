open System.Linq.Expressions
open System.Runtime.ExceptionServices
open System.Security.Cryptography.X509Certificates
//1.1
let g n = n + 4

//1.2
let h (x, y) = System.Math.Sqrt(x * x + y * y)

//1.3
//g = int -> int
//h = float * float -> float

//1.4
let rec f = 
    function 
    | 0 -> 0
    | n -> n + f (n - 1)

let f1 n = 
    let rec f1 = 
        function 
        | (0, y) -> y
        | (x, y) -> f1 (x - 1, y + 1)
    f1 (n, n)

//1.5
let rec fib = 
    function 
    | 0 | 1 as x -> x
    | x -> fib (x - 1) + fib (x - 2)

//1.6
let rec sum = 
    function 
    | (m, 0) -> m
    | (m, n) -> m + n + sum (m, n - 1)

let sum_tail (m, n) = 
    let rec sum_tail = 
        function 
        | (r, 0) -> r
        | (r, n) -> sum_tail (m + n + r, n - 1)
    sum_tail (m, n)

//1.7
// (System.Math.PI, fact -1) : float * int
// fact(fact 4) : int
// power(System.Math.PI, fact 2) : float
// (power, fact) : float -> int -> float * int -> int
//1.8
//f 3 = 4
//g 3 = 9


let rec alt_sum = 
    function 
    | [] -> 0
    | h :: t -> h - alt_sum t

let rec seq_of_pairs = 
    function 
    | h1 :: (h2 :: _ as t) -> (h1, h2) :: seq_of_pairs t
    | _ -> []

let rec sum_prod = 
    function 
    | [] -> (0, 1)
    | h :: t -> 
        let (h', t') = sum_prod t
        (h + h', h * t')

let rec foldl (f : 'a -> 'b -> 'a) (s : 'a) (l : 'b list) : 'a = 
    match l with
    | [] -> s
    | h :: t -> foldl f (f s h) t

let rec foldr (f : 'a -> 'b -> 'b) (s : 'b) (l : 'a list) : 'b = 
    match l with
    | [] -> s
    | h :: t -> f h (foldr f s t)

let rec reverse = 
    function 
    | [] -> []
    | h :: t -> 
        let t' = reverse t
        t' @ [h]

let reverse_tail l = 
    let rec reverse_tail' l a =
        match l with 
        | [] -> a
        | h :: t -> reverse_tail' t (h::a)
    reverse_tail' l []

let rec gdc x = 
    function
    | 0 -> x
    | y -> gdc y (x % y)

List.fold gdc 0 [88; 120; 240]

let positive_sum a b = 
    let a = System.Math.Max (a, 0)
    let b = System.Math.Max (b, 0)
    a + b

let give_me_a_three x = 2

let v = ref 0
v := 100
!v

type expr =
    | Plus of expr * expr
    | Times of expr * expr
    | Product of expr list
    | Value of string

let rec print_expr expr =
    match expr with
    | Plus (x, y) ->  "(" + print_expr x + " + " + print_expr y + ")"
    | Times (x, y) -> "(" + print_expr x + " * " + print_expr y + ")"
    | Product x ->
        match x with
        | [] -> ""
        | [e] -> print_expr e
        | h::t -> print_expr (Times (h, Product t))
    | Value x -> x

let rec print_expr expr =
    match expr with
    | Plus (x, y) ->  "(" + print_expr x + " + " + print_expr y + ")"
    | Times (x, y) -> "(" + print_expr x + " * " + print_expr y + ")"
    | Product x ->
        match x with
        | [] -> ""
        | [x] -> print_expr x
        | _ ->
            let rec print_expr' x =
                match x with
                | [] -> ""
                | [x] -> print_expr x
                | x::y -> print_expr x + " * " + print_expr' y
            "(" + print_expr' x + ")"
    | Value x -> x

let rec print_expr expr =
    match expr with
    | Plus (x, y) ->  "(" + print_expr x + " + " + print_expr y + ")"
    | Times (x, y) -> "(" + print_expr x + " * " + print_expr y + ")"
    | Product x ->
        match x with
        | [] -> ""
        | [x] -> print_expr x
        | x::y ->
            "(" + print_expr x + " * " + print_expr (Product y) + ")"
    | Value x -> x

print_expr (Product [Value "x"; Value "z"; Value"y"])
print_expr (Product [Value "x"])
print_expr (Product [])

let rec is_even x = 
    x = 0 || is_odd(x - 1) 
and is_odd x =
    x <> 0 && is_even(x - 1)

let is_unit () = true

match (fun i -> i + 1) with
    | 1 -> 2
    | _ -> 3

is_unit ();
let x y = y + 1
x begin 1 * 2 end

let a = 2 in a+1

let x = 1 in
let f y = x in
let x = 2 in
    f 0

let f x = x - 1 in
let f x = f (x - 1) in
    f 2



snd (1,2)

print_expr (Value "x")


let identity x = x
let identity' = (fun x -> (identity identity) x)
identity' 1
identity' "x"

let adddg x y = x + y
let result = adddg 1.3 2.1


