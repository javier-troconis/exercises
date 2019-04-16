open System.Xml.Xsl
open System.Security.Policy
open System.Drawing
open System

[ [ [ //2.1.7a 7
      //2.1.7c 2
      //2.1.7e false
      //2.1.7g 294
      //2.1.2a / must be applied to reals
      //2.1.2c and is used for dependent types
      //2.1.2e real numbers must have real part
      //2.4.1a 4
      //2.4.1c [4,5]
      //2.4.1e "foo"
      //2.4.1g ["c","o","b","o","l"]
      //2.4.2a tuple doesn't have 4th element
      //2.4.2c (1) is not a tuple
      //2.4.2g [] is an empty list - doesn't have tail
      //2.4.2i arguments must be list of strings
      //2.4.3a (float * (string * int list))
      //2.4.3c (int * float) list
      //2.4.4 (1,2) and (1,2,3) are not the same type - [1,2] and [1,2,3] are the same type
      //2.4.5a
      1 ] ] ]
([ //2.4.5c
   "x" ], (1, ("1.0", "x")), 1)
((//2.4.5e
  true, 1), 'x')

//example 3.16
let rec merge =
    function
    | (xs, []) -> xs
    | ([], xs) -> xs
    | ((x1 :: xs1 as xs1'), (x2 :: xs2 as xs2')) ->
        if x1 > x2 then x2 :: merge (xs1', xs2)
        else x1 :: merge (xs1, xs2')

let rec split =
    function
    | [] -> ([], [])
    | [ x ] -> ([ x ], [])
    | x :: x1 :: xs ->
        let (x2, x3) = split (xs)
        (x :: x2, x1 :: x3)

let rec merge_sort =
    function
    | [] -> []
    | [ x ] -> [ x ]
    | x ->
        let (x1, x2) = split x
        merge (merge_sort x1, merge_sort x2)

//example 3.19
let rec sum_lists =
    function
    | [] -> 0
    | [] :: xs -> sum_lists xs
    | (x :: xs) :: xs' -> x + sum_lists (xs :: xs')

//3.2.1b
let rec skip =
    function
    | ([], _) -> []
    | (_ :: xs as xs', n) ->
        if n = 0 then xs'
        else skip (xs, n - 1)

let rec take =
    function
    | ([], _) -> []
    | (x :: xs, n) ->
        if n = 0 then []
        else x :: take (xs, n - 1)

let rec cat =
    function
    | ([], m) -> m
    | (x :: xs, m) -> x :: cat (xs, m)

let cycle (l, n) = cat (skip (l, n), take (l, n))

//3.2.1c
let rec duplicate =
    function
    | [] -> []
    | x :: xs -> x :: x :: duplicate xs

//3.2.1f
//tail
let largest =
    function
    | [] -> None
    | x :: xs ->
        let rec largest =
            function
            | (n, []) -> Some n
            | (n, x :: xs) ->
                largest ((if n > x then n
                          else x), xs)
        largest (x, xs)

let rec largest1 l =
    if List.isEmpty l then None
    else if List.isEmpty (List.tail l) then Some(List.head l)
    else
        let x = List.head l
        let x1 = List.head (List.tail l)
        largest1 (if x > x1 then x :: List.tail (List.tail l)
                  else List.tail l)

let rec largest2 =
    function
    | [] -> None
    | [ x ] -> Some x
    | x :: x1 :: xs ->
        largest2 ((if x > x1 then x
                   else x1)
                  :: xs)

let rec largest3 =
    function
    | [] -> None
    | x :: xs ->
        match largest3 xs with
        | None -> Some x
        | Some x1 ->
            if x > x1 then Some x
            else Some x1

//3.3.1a
let rec factorial =
    function
    | 0 -> 1
    | 1 -> 1
    | n -> n * factorial (n - 1)

//3.3.5a x=a y=b zs=[c]
//3.3.5c no
//3.3.7
let rec square =
    function
    | 0 -> 0
    | n -> square (n - 1) + 2 * n - 1

//3.3.8
let rec order_pairs =
    function
    | [] -> []
    | (x1, x2) as x3 :: xs ->
        if x1 > x2 then (x2, x1) :: order_pairs xs
        else x3 :: order_pairs xs

//3.3.11a
let rec _member s =
    function
    | [] -> false
    | x :: xs ->
        if x = s then true
        else _member s xs

//3.3.11b
let rec delete s =
    function
    | [] -> []
    | x :: xs ->
        if x = s then xs
        else x :: delete s xs

//3.3.11c
let rec insert s =
    function
    | [] -> [ s ]
    | x :: xs as xs' ->
        if x = s then xs'
        else x :: insert s xs

//3.3.12
let rec _insert s =
    function
    | [] -> []
    | [ x ] -> [ s :: x ]
    | x :: xs -> (s :: x) :: _insert s xs

//3.5.1
let rec cat =
    function
    | ([], m) -> m
    | (x :: xs, m) -> x :: cat (xs, m)

//3.5.2
//example 4.3
let rec print_list =
    function
    | [] -> ()
    | x :: xs ->
        printf "%i" x
        printf "\n"
        print_list xs

//4.1.1
//no
//4.1.2
let rec comb =
    function
    | (_, 0) -> 1
    | (n, m) ->
        if n = m then 1
        else
            let r1 = comb (n - 1, m)
            let r2 = comb (n - 1, m - 1)
            r1 + r2

//4.1.3
let y n =
    let rec y r =
        function
        | 0 -> r
        | n -> y (r + r) (n - 1)
    printfn "%s" (y "X" n)

//4.2.3a
//option unit
//4.2.3c
//option option
//4.2.3e
//option->int
let rec foldl f s =
    function
    | [] -> s
    | x :: xs -> foldl f (f x s) xs

let rec foldr f l s =
    match l with
    | [] -> s
    | x :: xs -> f x (foldr f xs s)

//5.4.12b
List.foldBack (fun x s -> (x :: (List.head s)) :: s) [ 1; 2; 3 ] [ [] ]

let rec reverse_1 x =
    if x = [] then x
    else reverse_1 (List.tail x) @ [ List.head x ]

let rec reverse_2 =
    function
    | [] -> []
    | x :: xs -> reverse_2 xs @ [ x ]

type tree<'t> =
    | Empty
    | Node of ('t * 't tree * 't tree)

let t =
    Node
        (//6.4.1a
         //6.4.1b
         (*
Depth First Traversals:
(a) Inorder (Left, Root, Right) : 4 2 5 1 3
(b) Preorder (Root, Left, Right) : 1 2 4 5 3
(c) Postorder (Left, Right, Root) : 4 5 2 3 1

Breadth First or Level Order Traversal : 1 2 3 4 5
Please see this post for Breadth First Traversal.
*)



(*
type
    'a even_tree =
        Empty | EvenNode of ('a * 'a odd_tree * 'a odd_tree)
and
    'a odd_tree =
        OddNode of ('a * 'a even_tree * 'a even_tree)
*)

         "ml", Node("as", Node("a", Empty, Empty), Node("in", Empty, Empty)),
         Node("types", Empty, Empty))

let rec lookup v =
    function
    | Empty -> false
    | Node(t, l, r) ->
        if t > v then lookup v l
        else if v > t then lookup v r
        else true

let rec is_bst =
    function
    | Node(t, (Node(lt, _, _) as l), r) ->
        if lt > t then false
        else is_bst l && is_bst r
    | Node(t, l, (Node(rt, _, _) as r)) ->
        if t > rt then false
        else is_bst r && is_bst l
    | Node(_, Empty, Empty) -> true
    | Empty -> true

let rec insert t =
    function
    | Empty -> Node(t, Empty, Empty)
    | Node(t', l, r) as s ->
        if t > t' then Node(t', l, (insert t r))
        else if t' > t then Node(t', (insert t l), r)
        else s

let rec get_level =
    function
    | Empty -> 0
    | Node(_, l, r) ->
        let l' = get_level l
        let r' = get_level r
        1 + (if l' > r' then l'
             else r')

get_level t

let shuffle l = List.sortWith (fun x y -> System.Guid.NewGuid().GetHashCode()) l

lookup "in" t
is_bst t
t
|> insert "z"
|> is_bst

let t2 =
    [ 1..3..50 ]
    |> shuffle
    |> List.fold (fun s e -> insert e s) Empty

get_level t2

type gtree<'t> = Node of ('t * 't gtree list)

let rec sum_all_labels =
    function
    | Node(t, []) -> t
    | Node(t, x :: xs) -> sum_all_labels x + sum_all_labels (Node(t, xs))

let rec find t =
    function
    | Node(t', []) -> t' = t
    | Node(t', x :: xs) -> find t x || find t (Node(t', xs))

let rec find t (Node(t', x)) =
    t = t' || List.fold (fun s e -> s || find t e) false x

type person_record =
    { name : string }

type person_record_1 =
    { name : string }

let c : person_record = { name = "dewdw" }
let d : person_record_1 = { name = "dewdw" }
let get_name { name = b } = printfn "%s" b

get_name d

let rec quicksort_sequential =
    function
    | [] -> []
    | x :: xs ->
        let smaller, larger = List.partition (fun number -> number < x) xs
        quicksort_sequential smaller @ (x :: quicksort_sequential larger)

quicksort_sequential [ 7; 3; 9; 2 ]

let split_evenly l = List.fold (fun (l1, l2) x -> (l2, x :: l1)) ([], []) l

split_evenly [ 1; 2; 3; 4 ]



type expr =
    Cst of int
    | Var of string
    | Let of string * expr * expr
    | Expr of string * expr * expr

let rec find y = function
    [] -> raise (System.Exception "variable not found")
    | (k, v)::xs -> if k = y then v else find y xs

let rec eval env =
    function
    Cst x -> x
    | Var x -> find x env
    | Let (variable_name, e1, e2) -> 
        let variable_value = eval env e1
        eval ((variable_name, variable_value)::env) e2
    | Expr(operator, e1, e2) ->
        match operator with
        "+" -> (+) (eval env e1) (eval env e2)
        | "-" -> (-) (eval env e1) (eval env e2)
        | _ -> raise (System.Exception "unknown operator")

eval [("x", 2)] (Let ("x", Var "x", (Expr ("-", Cst 1, Var "x"))))
