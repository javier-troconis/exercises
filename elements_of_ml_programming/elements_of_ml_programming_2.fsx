let rec explode = function
    | "" -> []
    | x -> x.[0] :: explode x.[1..]

let rec implode = function
    | [] -> ""
    | x1::x2 -> sprintf "%c%s" x1 (implode x2)

let rec take = function
    | [] -> []
    | x1::x2 -> x1 :: skip x2
    and
    skip = function
    | [] -> []
    | _::x2 -> take x2

let rec merge = function
    | (x1, []) -> x1
    | ([], x1) -> x1
    | ((x1::x2 as x3), (x4::x5 as x6)) -> 
        if x1 > x4 then x4 :: merge (x3, x5) 
        else x1 :: merge (x2, x6)

let rec sum_list = function
    | [] -> 0
    | []::x3 -> sum_list x3    
    | (x1::x2)::x3 -> x1 + sum_list (x2::x3)

let rec split = function
    | x1::x2::x3 -> 
        let (x4,x5) = split x3
        (x1::x4, x2::x5)
    | [x1] -> ([x1], [])
    | [] -> ([],[])

let rec merge_sort = function
    | [] -> []
    | [x1] -> [x1]
    | x1 -> 
        let (x2,x3) = split x1
        merge (merge_sort x2, merge_sort x3) 

let reverse_no_backtracking x1 = 
    let rec reverse_no_backtracking r = function
        | [] -> r
        | x1::x2 -> reverse_no_backtracking (x1::r) x2
    reverse_no_backtracking [] x1

let rec fold_right f l s =
    match l with
    | [] -> s
    | x1::x2 -> f x1 (fold_right f x2 s)

let fold_right_cps f l s =
    let rec foldright_cps f1 l s =
        match l with
        | [] -> f1 s
        | x1::x2 -> foldright_cps (fun x3 -> f1 (f x1 x3)) x2 s
    foldright_cps id l s


//2.1.1.a 7
//2.1.1.c 2
//2.1.1.e false
//2.1.1.g 294
//2.1.2.a '/' cannot be applied to 'int', must use 'div'
//2.1.2.c 'and' is not a logical operator
//2.1.2.e left operand missing decimal digits
//2.1.1.g this would add the char ascii codes and produce the char equivalent to the sum of the ascii codes.
//2.1.1.i cannot test equality on 'float' using = due to rounding, must use >=, <=
//2.1.4.a if E then true else F
//2.1.4.b if E then F else false
let ``2.4.5.a`` = [[[0];[]]; [[];[]]]
let ``2.4.5.c`` = ([0], (0, (0.0, "")), 0)
let ``2.4.5.f`` = (1.0, [[[[1]; []]; [[]; []]]; [[[]; []]; [[]; []]]]) 
let ``2.4.6`` = List.head (explode "a")
let ``3.1.1.b`` (a,b,c) = 
    if a > b then if b > c then c else b
    else if a > c then c else a
let ``3.1.1.c`` x = List.head (List.tail (List.tail x))
let ``3.1.1.f`` x = List.tail x @ [List.head x]
let ``3.1.2.a`` (a,b,c) = 
    if (a > b) then
        if b > c then (a, c) else if a > c then (a, b) else (c, b)
    else
        if a > c then (b, c) else if c > b then (c, a) else (b, a)
let ``3.1.2.b`` (a,b,c) =  
    if (a > b) then
        if b > c then [a; b; c] else if a > c then [a; c; b] else [c; a; b]
    else
        if a > c then [b; a; c] else if c > b then [c; b; a] else [b; c; a]
let ``3.1.2.d`` x = List.head x :: List.tail (List.tail x)
//3.1.3.a 8
//3.1.3.b 11
//3.1.3.e 18
let rec ``3.2.1.a`` x = 
    if x > 1 then x * ``3.2.1.a`` (x - 1) else 1
let rec ``3.2.1.b`` (l, i) =
    if i > 0 then ``3.2.1.b`` (``3.1.1.f`` l, i - 1) else l
let rec ``3.2.1.c`` x = 
    if List.isEmpty x then x 
    else List.head x :: List.head x :: ``3.2.1.c`` (List.tail x)
let rec ``3.2.1.e`` (x:float, i) = if i > 0 then x * ``3.2.1.e`` (x, i - 1) else 1.0
let rec ``3.2.1.f`` x = 
    if List.isEmpty x then 0 
    else 
        let x1 = List.head x
        let x2 = ``3.2.1.f`` (List.tail x)
        if x1 > x2 then x1 else x2
//3.2.2 the expression c + 1 makes a, b, c, d all int
//3.2.3.a a,b,c = int; d,e = any
//3.2.3.c a,b,c,d,e = int
//3.2.3.d a,b,c = int, d = bool
//3.2.3.f b,c = int; d,e = any
//3.2.3.g b,c,d,e = int
let rec ``3.3.2`` = function
    | x1::x2::x3 -> x2::x1::``3.3.2`` x3
    | x1 -> x1
let rec ``3.3.3`` i = function
    | [] -> []
    | x1::x2 -> 
        if i = 0 then x2
        else x1 :: ``3.3.3`` (i-1) x2
//3.3.5.a yes
//3.3.5.b yes
//3.3.5.c no
let rec ``3.3.7`` x = 
    if x > 0 then ``3.3.7`` (x - 1) + 2 * x - 1 
    else 0
let rec ``3.3.8`` = function
    | [] -> []
    | ((x1, x2) as x3)::x4 -> (if x2 > x1 then x3 else (x2, x1)) :: ``3.3.8`` x4
let rec ``3.3.9`` = function
    | 'a'::_ -> true
    | 'e'::_ -> true
    | 'i'::_ -> true
    | 'o'::_ -> true
    | 'u'::_ -> true
    | _::x1 -> ``3.3.9`` x1
    | [] -> false
let rec ``3.3.11.a`` x1 = function
    | [] -> false
    | x2::x3 -> if x1 <> x2 then ``3.3.11.a`` x1 x3 else true
let rec ``3.3.11.b`` x1 = function
    | [] -> []
    | x2::x3 -> if x1 <> x2 then x2::``3.3.11.b`` x1 x3 else x3
let rec ``3.3.11.c`` x1 = function
    | [] -> [x1]
    | x2::x3 as x4 -> if x1 <> x2 then x2::``3.3.11.c`` x1 x3 else x4
let rec ``3.3.12`` a = function
    | [] -> []
    | x1::x2 -> (a::x1)::``3.3.12`` a x2
let rec ``3.3.13`` = function
    | [] -> [[]]
    | x1::x2 -> 
        let x3 = ``3.3.13`` x2
        ``3.3.12`` x1 x3 @ x3
let rec ``3.3.14.1`` x1 = function
    | x2::x3 -> x1 - x2 * ``3.3.14.1`` x1 x3
    | [] -> 1.0
let rec ``3.3.14`` = function
    | x1::x2 -> ``3.3.14.1`` x1 x2 * ``3.3.14`` x2
    | [] -> 1.0
let ``3.4.1`` x = 
    let x = x * x * x * x * x
    let x = x * x * x * x * x
    let x = x * x * x * x * x
    x * x * x * x * x * x * x * x
let ``3.4.2`` = split
let ``3.4.3`` = ``3.3.13``
let ``3.4.4`` = ``3.2.1.f``
let rec ``3.4.5`` (x,i) =
    if i > 0 then x * x * ``3.4.5`` (x,i-1) else 1
let rec ``3.4.6`` = function
    | [] -> (0,0)
    | (x1,x2)::x3 -> 
        let (x4,x5) = ``3.4.6`` x3
        (x1 + x4, x2 + x5)
let rec ``3.4.7`` = function
    | [] -> (0,0)
    | [x1] -> (0,x1)
    | x1::x2::x3 -> 
        let (x4,x5) = ``3.4.7`` x3
        (x2 + x4, x1 + x5)
let rec ``3.5.1`` = function
    | ([], x1) -> x1
    | (x1::x2, x3) -> x1::``3.5.1`` (x2,x3)
let ``3.5.1.cps`` (x1,x2) =
    let rec ``3.5.1.cps`` f = function
        | ([], x1) -> f x1
        | (x1::x2, x3) -> ``3.5.1.cps`` (fun x4 -> f (x1::x4)) (x2,x3)
    ``3.5.1.cps`` id (x1,x2)
