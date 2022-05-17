type Label = string

type Code = string

type Price = int

type Inventory = (Code * (Label * Price)) list

type Quantity = int

type PurchaseItem = Code * Quantity

type Purchase = PurchaseItem list

type BillItem = Label * Quantity * Price

type Bill = BillItem list * Price

type MakeBill = Inventory -> Purchase -> Bill

let inventory = 
    [ ("a", ("product a", 1))
      ("b", ("product b", 2))
      ("c", ("product c", 3)) ]

let purchase = 
    [ ("a", 4)
      ("c", 1)
      ("b", 2) ]

let rec find_product code = 
    function 
    | (x, y) :: _ when x = code -> y
    | _ :: t -> find_product code t
    | _ -> failwith "failed to find product"

let rec make_bill inventory = 
    function 
    | [] -> ([], 0)
    | (code, quantity) :: t -> 
        let (c, d) = make_bill inventory t
        let (description, price) = find_product code inventory
        (description, quantity, price * quantity) :: c, quantity * price + d

let length x = List.fold (fun s _ -> s + 1) 0 x

let f x = 
    if x then failwith "error in true branch"
    else failwith "error in false branch"

let fold_back f s l = List.fold (fun s e -> f s e) s l

let rec pow = 
    function 
    | (_, 0) -> 1
    | (x, y) -> x * pow (x, y - 1)

let rec pow1 x = 
    function 
    | 0 -> 1
    | y -> x * pow1 x (y - 1)

let rec pow2 x y = 
    match (x, y) with
    | (_, 0) -> 1
    | (x, y) -> x * pow2 x (y - 1)

let rec pow3 x y = 
    match y with
    | 0 -> 1
    | y -> x * pow3 x (y - 1)

type expr<'a> = 
    | True
    | False
    | And of 'a expr * 'a expr
    | Or of 'a expr * 'a expr
    | Not of 'a expr
    | Base of 'a

let rec eval eval_base expr = 
    let eval' x = eval eval_base x
    match expr with
    | True -> true
    | False -> false
    | Base x -> eval_base x
    | And(x, y) -> eval' x && eval' y
    | Or(x, y) -> eval' x || eval' y
    | Not x -> not (eval' x)

eval (fun x -> x = 1) (And(Base 2, Base 3))

let compute_bounds l = 
    let l' = List.sort l
    (List.head l', List.last l')

let rec compute_bounds1 = 
    function 
    | [] -> 0, 0
    | [ x ] -> x, x
    | h :: t -> 
        let x, y = compute_bounds1 t
        (System.Math.Min(h, x), System.Math.Max(h, y))

let compute_bounds2 cmp list = 
    let sorted = List.sortBy cmp list
    match List.head sorted, List.last sorted with
    | None, _ | _, None -> None
    | Some x, Some y -> Some(x, y)

let rec alt_sum = 
    function 
    | [] -> 0
    | [ x ] -> x
    | h1 :: h2 :: t -> h1 - h2 + alt_sum t

let rec alt_sum1 = 
    function 
    | [] -> 0
    | h1 :: t1 -> 
        match t1 with
        | [] -> h1
        | h2 :: t2 -> h1 - h2 + alt_sum1 t2

let rec convert_to_base symbols number = 
    let base' = Array.length symbols
    if base' > number then string symbols.[number]
    else convert_to_base symbols (number / base') + string (number % base')

let rec sum_prod = 
    function 
    | [] -> (0, 1)
    | h :: t -> 
        let a, b = sum_prod t
        (h + a, h * b)

let rec append a b = 
    match a with
    | [] -> b
    | h :: t -> h :: (append t b)

let rec drop_value l v = 
    match l with
    | [] -> []
    | h :: t -> 
        let t' = drop_value t v
        if h = v then t'
        else h :: t'

let rec foldl f l s = 
    match l with
    | [] -> s
    | h :: t -> foldl f t (f s h)

let rec foldr (f : 'a -> 'b -> 'b) (acc : 'b) (list : 'a list) = 
    match list with
    | [] -> acc
    | h :: t -> f h (foldr f acc t)