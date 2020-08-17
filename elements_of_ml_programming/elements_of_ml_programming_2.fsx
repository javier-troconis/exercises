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
    | x1::x2 -> take x2

let rec merge = function
    | (x1, []) -> x1
    | ([], x1) -> x1
    | ((x1::x2 as x3), (x4::x5 as x6)) -> 
        if x1 > x4 then x4 :: merge (x3, x5) 
        else x1 :: merge (x2, x6)

let rec sumList = function
    | [] -> 0
    | []::x3 -> sumList x3    
    | (x1::x2)::x3 -> x1 + sumList (x2::x3)

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
