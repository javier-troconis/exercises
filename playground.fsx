
let x (a,b,c) = ((if a > b then (if a > c then a else c) else (if b > c then b else c)), (if a < b then (if a < c then a else c) else (if b < c then b else c)))

let rec x = function
    | [] -> []
    | h::t -> x t @ [h]

let rec x l = if(List.isEmpty l) then [] else x (List.tail l) @ [(List.head l)]

let rec comb (a,b) =
    if(a = 0 || a = b) 
    then 1
    else comb(a - 1, b) + comb(a - 1, b - 1) 

let rec x = function
    | [] -> []
    | h::t -> h :: y t

and y = function
    | [] -> []
    | h::t -> x t

let x f l =
    match l with
    | [] -> []
    | h::t -> h :: f t

let rec y l =
    match l with
    | [] -> []
    | h::t -> x y t

let rec x = function
    | (a,b) when a > b -> []
    | (a,b) -> a :: x (a + 1, b)

let rec x l =
    function
    | (a,b) when a > b -> l
    | (a,b) -> x (l @ [a]) (a + 1, b)

let rec x n = 
    if n <= 1 then 1
    else n * x (n - 1)

let rec reverse = function
    | [] -> []
    | h::t -> reverse t @[h]

let rec merge x y =
    match (x,y) with
    | ([], y) -> y
    | (x, []) -> x
    | (xh::xt, yh::yt) -> 
        if(xh > yh) then yh :: merge x yt 
        else xh :: merge xt y

let rec sumLists = function
    | [] -> 0
    | []::xs -> sumLists xs
    | (x::xs) :: y -> x + sumLists (xs :: y)

let rec length = function
    | [] -> 0
    | _::xs -> 1 + length xs

let rec flip = function
    | [] -> []
    | [x] -> [x]
    | x::y::ys -> y :: x :: flip ys

let rec square = function
    | 0 -> 0
    | x -> square (x - 1) + 2 * x - 1

let hundredthPower x =
    let x = x * x * x * x in
    let x = x * x * x * x in
    x * x * x * x * x 

let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x1::x2::xs -> 
        let (x3, x4) = split xs
        (x1::x3, x2::x4)

let rec merge = function
    | ([], x) -> x
    | (x, []) -> x
    | ((x1::x1s as x), (y1::y1s as y)) ->
        if(x1 > y1) then y1 :: merge (x, y1s) 
        else x1 :: merge (x1s, y)

let rec mergeSort = function
    | [] -> []
    | [x] -> [x]
    | x ->
        let (x1,x2) = split x
        let x3 = mergeSort x1
        let x4 = mergeSort x2
        merge (x3, x4)

let reverse x =
    let rec reverse = function
    | ([], x) -> x
    | (x::xs, y) -> reverse (xs, x::y)
    reverse (x, [])

let rec cat = function
    | ([], y) -> y
    | (x::xs, y) -> x :: cat (xs, y)

let rec skip = function
    | (xs, 0) -> xs
    | (_::xs, c) -> skip(xs, c - 1)

let rec take = function
    | (_, 0) -> []
    | (x::xs, c) -> x :: take(xs, c - 1)
    
let cycle (l, i) =
    cat (skip (l, i), take (l, i))

let rec pow = function
    | (_, 0) -> 1.0
    | (n:float, e) -> n * pow (n, e - 1)

let reverse_number n =
    let rec reverse_number = function
        | (0, r) -> r
        | (n, r) -> 
            let r = n % 10 + r * 10
            if n >= 0 && r < 0 || 0 > n && r >= 0 then 0
            else reverse_number (n / 10, r)
    reverse_number (n, 0)

let reducel f = function
    | [] -> failwith "cannot reducel empty list"
    | [x] -> x
    | x::x1::xs -> reducel f (f x x1::xs)

let curry f a b = f(a, b)

let uncurry f (a, b) = f a b

let rec applyList fs v =
    match fs with
    | [] -> []
    | x::xs -> x v :: applyList xs v


List.fold (fun s e -> s + e.ToString()) "" ['a'; 'b']

List.foldBack (fun e s -> e.ToString() + s) ['a'; 'b'] ""

type mapping<'d, 'f> = ('d *'f) list
([(1,2)]:mapping<int, int>) 

type X<'a, 'b> = 
    | A of 'a
    | B of ('a * 'b)

let rec Y = function
    | [] -> 0
    | A(_)::xs -> Y xs
    | B(_, x)::xs -> x + Y xs

Y [A("a"); B("b", 1)]

type T2<'a, 'b> = ('a * 'b)

let c (b:T2<'a, 'b>) = fst b

c (1,2)

type Tree<'t> = 
    | Empty
    | Node of 't * Tree<'t> * Tree<'t>

let x  = Node ("a", Node("b", Empty, Node("c", Empty, Empty)), Node("d", Empty, Empty))
let x1 = (Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty)))

let rec t = function
    | Empty -> []
    | Node(a,b,c) -> a::(t b)@(t c)


let rec t1 s = function
    | Empty -> s
    | Node(a,b,c) -> 
        t1 (a::(t1 s c)) b


let in_order tree =
  let rec aux tree accumulator =
    match tree with
    | Empty -> accumulator
    | Node (value, left, right) -> 
      let acc_with_right = aux right accumulator in
      let acc_with_value = value :: acc_with_right in
      aux left acc_with_value in
  aux tree []     

t1 [] x

type EvenTree<'t> = Empty | EvenNode of 't * OddTree<'t> * OddTree<'t>
and OddTree<'t> = OddNode of 't * EvenTree<'t> * EvenTree<'t>

type BinaryTree<'t> = Leaf | Node of 't * BinaryTree<'t> * BinaryTree<'t>

let rec isBsn = function
    | Leaf -> true
    | Node(_, Leaf, Leaf) -> true
    | Node(t, Node(t1, t2, t3), t4) -> t > t1 && isBsn t2 && isBsn t3 && isBsn t4
    | Node(t, t1, Node(t2, t3, t4)) -> t2 > t && isBsn t1 && isBsn t3 && isBsn t4

let rec searchBsn gt tree term =
    match tree with
    | Leaf -> false
    | Node(t, t1, t2) -> if gt(term, t) then searchBsn gt t2 term else if gt(t, term) then searchBsn gt t1 term else true

let rec insertBsn gt tree term =
    match tree with
    | Leaf -> Node(term, Leaf, Leaf)
    | Node(t, t1, t2) -> 
        if gt(term, t) then Node(t, t1, insertBsn gt t2 term)
        else if gt(t, term) then Node(t, insertBsn gt t1 term, t2)
        else tree

let t = Node("ml",
            Node("as",
                Node("a", Leaf, Leaf),
                Node("in", Leaf, Leaf)
            ),
            Node("types", Leaf, Leaf)
        )

isBsn t

searchBsn (fun (t1, t2) -> t1 > t2) t "ml"

type 't Result = 
    | Ok of 't
    | Error of string

let bind f = function
    | Ok t -> f t
    | Error _ as error -> error 

let map f = function
    | Ok t -> Ok (f t)
    | Error _ as error -> error 

let (|>>) x f = bind f x
let (|>>>) x f = map f x

let s1 (s:string) = if String.length s % 2 = 0 then Ok s else Error "odd"
let s2 (s:string) = s

Ok "3" |>> s1

"1" |> s1 |>>> s2 |>> s1

let sum_cps n = 
    let rec f f' = function
    | 0 -> f' 0
    | n1 -> f (fun n2 -> f' (n1 - n2)) (n1 - 1)
    f id n

let sum n = 
    let rec f r = function
    | 0 -> r
    | n -> f (n + r) (n - 1)
    f 0 n

sum_cps 2


let rec cumsum c f = function
 | [] -> f []
 | x::x1 -> cumsum (x + c) (fun x2 -> f ((x + c)::x2)) x1

cumsum 0 id [1;2;3;4;5;6]

let rec foldl f s = function
 | [] -> s
 | x::x1 -> foldl f (f s x) x1

let rec foldr f l s =
    match l with
    | [] -> s
    | x::x1 -> f x (foldr f x1 s)

let rec foldr1 fc f l s =
    match l with
    | [] -> fc s
    | x::x1 -> foldr1 (fun s1 -> fc (f x s1)) f x1 s

foldl (fun s i -> i::s) [] [1..1000000]
foldr (fun i s -> i::s) [1..1000000] [] 
foldr1 id (fun i s -> i::s) [1..100000000] [] 
List.foldBack (fun i s -> i::s) [1..100000000] [] 


let rec convert_to_system (symbols:string[]) n = 
    if (n > Array.length symbols - 1)
    then 
        let q = n / Array.length symbols
        let r = n % Array.length symbols
        convert_to_system symbols q + symbols.[r]
    else 
        symbols.[n]
    
convert_to_system [|"0";"1"|] 3


Seq.foldBack (fun e s -> e::s) "ab" []

type Step<'t> = 
    | Empty
    | Condition of string * ('t -> bool) * Step<'t> * Step<'t>
    | Activity of string * ('t -> 't) * Step<'t>

let findStepBFS<'t> step index =
    let queue = new System.Collections.Generic.Queue<Step<'t> * int>()
    let mutable result = Empty
    queue.Enqueue (step, 0)
    while queue.Count > 0 do
        let (step, index') = queue.Dequeue()
        if index = index' 
            then 
                result <- step
                queue.Clear()
            else
                match step with
                | Empty -> ()
                | Condition (_, _, l, r) -> 
                    queue.Enqueue (l, index' + queue.Count + 1)
                    queue.Enqueue (r, index' + queue.Count + 1)
                | Activity (_, _, n) ->
                    queue.Enqueue (n, index' + queue.Count + 1)
    result

let findStepDFS<'t> step index = 
    let rec findStepDFS step index' :(Step<'t> * int) =
        match step with
        | Empty -> (Empty, index')
        | Condition (_, _, l, r) -> 
            if index = index'
                then (step, index')
                else
                    match findStepDFS l (index' + 1) with
                    | (Empty, index') -> findStepDFS r (index' + 1)
                    | l -> l
        | Activity (_, _, n) ->
            if index = index'
                then (step, index')
                else 
                    findStepDFS n (index' + 1)
    let (step, _) = findStepDFS step 0
    step


let rec evaluateStep state = function 
    | Empty -> (state, Empty)
    | Condition (_, f, l, r) -> if f state then evaluateStep state l else evaluateStep state r
    | Activity (_, f, n) -> (f state, n)

let rec evaluateStepPath state step index =
    match findStepBFS step index with
    | Empty -> state
    | step ->
        let (state, step) = evaluateStep state step 
        evaluateStepPath state step 0

let rec step1 = Condition ("0", (fun s -> s <= 9), 
                            Activity("1", (+) 1, 
                                step1),
                            Empty)

findStepDFS step1 1000

evaluateStepPath 0 step1 0


type Expr =
    | Cst of int
    | Var of string
    | Let of string * Expr * Expr
    | Sum of Expr * Expr
    | Sub of Expr * Expr

let rec evaluate expr env =
    match expr with
    | Cst v -> v
    | Var k -> Map.find k env
    | Let (k, v, expr) -> 
        let v1 = evaluate v env
        let env1 = Map.add k v1 env
        evaluate expr env1
    | Sum (expr1, expr2) -> evaluate expr1 env + evaluate expr2 env
    | Sub (expr1, expr2) -> evaluate expr1 env - evaluate expr2 env

let env = Map.ofList [ ("a", 1) ]
let expr = Sub (
                Sum (Cst 2, Let ("a", Cst 2, Var "a")),  
                Var "a")
evaluate expr env