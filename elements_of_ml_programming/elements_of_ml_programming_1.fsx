let explode (x:char seq) = Seq.foldBack (fun e s -> e::s) x []
let rec merge = function
    | ([], x) -> x
    | (x, []) -> x
    | ((x1::x2 as x3), (x4::x5 as x6)) -> 
        if x1 > x4
        then x4 :: merge (x3, x5)
        else x1 :: merge (x2, x6)
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x1::x2::x3 -> 
        let (x4, x5) = split x3
        (x1::x4, x2::x5)
let rec mergesort = function
    | [] -> []
    | [x] -> [x]
    | x -> 
        let (x1, x2) = split x
        let x1 = mergesort(x1)
        let x2 = mergesort(x2)
        merge (x1, x2)
let reverse x =
    let rec reverse acc = function
    | [] -> acc
    | x1::x2 -> reverse (x1::acc) x2
    reverse [] x
let concat x =
    let rec concat f = function
    | ([], x) -> f x
    | (x1::x2, x3) ->  concat (fun x4 -> f (x1::x4)) (x2, x3)
    concat id x
let rec skip = function
    | ([], _) -> []
    | ((_::x2 as x3), i) -> if i >= 1 then skip (x2, i - 1) else x3
let take (l, i) =
    let rec take f (l, i) =
        if i >= 1
        then
            match l with
            | x1::x2 -> take (fun x3 -> f (x1::x3)) (x2, i - 1) 
            | x -> f x
        else 
            f []
    take id (l, i)

//2.1.1.a 7
//2.1.1.c 2
//2.1.1.e false
//2.1.1.g 294
//2.1.2.a the / operator applies to reals
//2.1.2.c the and operator cannot be used for booleans operands, use andalso instead
//2.1.2.e type mismatch
//2.1.2.g type mismatch
//2.1.2.i can't test reals for equality, should be x <= y && x >= y
//2.1.4.a if E then true else F
//2.2.1.a
System.Math.Floor 123.45 |> System.Convert.ToInt32
//2.2.1.d
System.Math.Ceiling -123.45 |> System.Convert.ToInt32
//2.2.1.e
System.Convert.ToInt32 'Y'
//2.2.1.g
'N' |> System.Convert.ToInt32 |> System.Convert.ToDouble
//2.2.1.h
97.0 |> System.Convert.ToInt32 |> System.Convert.ToChar
//2.2.2.a ceil domain is float
//2.2.2.c 256 is outside of the ascii range of 0-255
//2.2.2.e ord domain is char
//2.2.2.h ord domain is char
//2.4.1.a 4
//2.4.1.c [4; 5]
//2.4.1.e "foo"
//2.4.1.g ["c"; "o"; "b"; "o"; "l"]
//2.4.2.a #3(3, 4, 5)
//2.4.2.c 1
//2.4.2.e ('a'; 'b')
//2.4.2.g can't List.tail on empty list
//2.4.2.i concat works on string list
//2.4.3.a float * (string * (int list))
//2.4.3.c (int * float) list
//2.4.4 (1,2) is not the same type as (1,2,3). [1,2] is the same type as [1,2,3]
//2.4.5.a
[[[0]; []]]
//2.4.5.c
([""], (0, (0., "")), 0)
//2.4.5.e
((false, 0), '0') 
let ``3.1.1.a`` x :float = x * x * x
let ``3.1.1.b`` a b c = if a > b 
                        then 
                            if b > c 
                            then c 
                            else b
                        else 
                            if a > c
                            then c
                            else a
let ``3.1.1.c`` x = List.head (List.tail (List.tail x))
let ``3.1.1.e`` x = ``3.1.1.c`` (explode x)
let ``3.1.1.f`` x = List.tail x @ [List.head x]
let ``3.1.2.a`` a b c = if a > b 
                        then 
                            if b > c 
                            then (a, c) 
                            else ((if a > c then a else c), b)
                        else 
                            if a > c
                            then (b, c)
                            else ((if b > c then b else c), a)
//3.1.3.a 8
//3.1.3.b 11
//3.1.3.e 18
let rec ``3.2.1.a`` x = if x > 1 then x * ``3.2.1.a`` (x - 1) else 1
let rec ``3.2.1.b`` i l = if i = 0 then l else ``3.2.1.b`` (i - 1) (``3.1.1.f`` l)
let rec ``3.2.1.c`` l = if List.isEmpty l then l else List.head l ::  List.head l :: ``3.2.1.c`` (List.tail l)
let rec ``3.2.1.d`` l = if List.isEmpty l then 0 else 1 + ``3.2.1.d`` (List.tail l)
let rec ``3.2.1.f_backtracking`` l = 
                        if List.isEmpty (List.tail l) 
                        then List.head l 
                        else 
                            let x = List.head l
                            let y = ``3.2.1.f_backtracking`` (List.tail l)
                            if x > y then x else y
let rec ``3.2.1.f_no_backtracking`` l = 
                        if List.isEmpty (List.tail l) 
                        then List.head l 
                        else 
                            let x = List.head l
                            let y = List.head (List.tail l)
                            if x > y 
                            then ``3.2.1.f_no_backtracking`` (x :: List.tail (List.tail l)) 
                            else ``3.2.1.f_no_backtracking`` (List.tail l) 
//3.2.2 because of c+1
//3.2.3.a b and c are int
//3.2.3.c all are int
//3.2.3.f b and c are int
let rec ``3.3.1.a`` = function 
    | 1 -> 1 
    | x -> x * ``3.3.1.a`` (x - 1)
let rec ``3.3.1.d`` = function
    | [] -> []
    | x1::x2 -> x1::x1::``3.3.1.d`` x2
let rec ``3.3.1.f`` = function
    | [] -> failwith "empty list"
    | [x] -> x
    | x1::x2::x3 -> if x1 > x2 then ``3.3.1.f`` (x1::x3) else ``3.3.1.f`` (x2::x3)
//3.3.5.a yes, x=a, y=b, zs=[c], w=[d;e]
//3.3.5.c no
let rec ``3.3.7`` = function
    | 0 -> 0
    | x -> 2 * x - 1 + ``3.3.7`` (x - 1)
let rec ``3.3.8`` = function
    | [] -> []
    | ((x1, x2) as x3)::x4 -> if x1 > x2 then (x2, x1)::``3.3.8`` x4 else x3::``3.3.8`` x4
let rec ``3.3.11.c`` x = function
    | [] -> [x]
    | x1::x2 as x3 -> if x1 = x then x3 else x1::``3.3.11.c`` x x2
let rec ``3.3.12`` x = function
    | [] -> []
    | x1::x2 -> (x::x1)::``3.3.12`` x x2
let rec ``3.3.13`` = function 
    | [] -> [[]]
    | x1::x2  -> 
        let x3 = ``3.3.13`` x2
        x3 @ ``3.3.12`` x1 x3
let ``3.4.1`` x = 
    let x5 = x * x * x * x * x
    let x25 = x5 * x5 * x5 * x5 * x5
    let x100 = x25 * x25 * x25 * x25
    let x500 = x100 * x100 * x100 * x100 * x100
    x500 * x500
let rec ``3.4.5`` x = function
    | 0 -> x
    | i -> 
        let x1 = ``3.4.5`` x (i - 1)
        x1 * x1
let rec ``3.4.7`` = function
    | [] -> (0, 0)
    | [x] -> (x, 0)
    | x1::x2::x3 -> 
        let (x4, x5) = ``3.4.7`` x3
        (x1 + x4, x2 + x5)
let ``3.5.1_no_backtracking`` x =
    let rec ``3.5.1`` f = function
    | ([], x) -> f x
    | (x1::x2, x3) ->  ``3.5.1`` (fun x4 -> f (x1::x4)) (x2, x3)
    ``3.5.1`` id x
let rec ``3.5.1_backtracking`` = function
    | ([], x) -> x
    | (x1::x2, x3) ->  x1 :: ``3.5.1_backtracking`` (x2, x3)
let ``3.5.2`` (l, i) =
    concat (skip (l, i),  take(l, i))
//4.1.1 no
let ``4.3.1`` n = 
    let rec ``4.3.1`` n =
        if n >= 1
        then
            let r = ``4.3.1`` (n - 1) 
            r + r
        else
            "x"
    printfn "%s" (``4.3.1`` n)
let ``4.3.1_1`` n = 
    let rec ``4.3.1`` r n =
        if n >= 1
        then
            ``4.3.1`` (r + r) (n - 1)
        else
            r
    printfn "%s" (``4.3.1`` "x" n)

