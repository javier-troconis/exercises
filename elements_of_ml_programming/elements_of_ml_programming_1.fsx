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