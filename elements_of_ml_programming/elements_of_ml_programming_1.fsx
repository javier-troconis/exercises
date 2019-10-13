let explode (x:char seq) = Seq.foldBack (fun e s -> e::s) x []

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
