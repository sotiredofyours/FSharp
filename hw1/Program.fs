let rec factorial x =
    if x = 1 then 1
    else x * factorial(x-1)
    
let rec fibonacci x =
    if x <= 2  then 1 
    else fibonacci(x-1)+fibonacci(x-2)
    
let powers n m = 
    let rec pow x y =
        if y = 1 then x
        else x * pow(x)(y-1)
    [for x in n..m -> pow(2)(x)]
    
let rec reverse list =
    match list with
    |[] -> []
    |[x] -> [x]
    | head::tail -> reverse tail @ [head]
    
let rec findElem list x acc =
    match list with
    | [] -> -1
    |head::tail -> if head = x then acc else findElem(tail)(x)(acc+1)
   





    
    