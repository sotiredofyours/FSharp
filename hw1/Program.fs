let factorial n =
    let rec loop i acc =
        match i with
        | 1 -> acc
        | _ -> loop(i-1)(acc*i)
    loop n 1
    
let fibonacci x =
    let rec loop acc1 acc2 n =
        match n with
        | 0 -> acc1
        | 1 -> acc2
        | _ -> loop acc2 (acc1+acc2) (n-1)
    loop 0 1 x
    
let powers n m = 
    let rec pow x y =
        if y = 1 then x
        else x * pow(x)(y-1)
    [for x in n..m -> pow(2)(x)]
    
let rec reverse list acc =
    match list with
    |[] -> acc
    | head::tail -> reverse(tail)(head::acc)
    
let rec findElem list x acc =
    match list with
    | [] -> -1
    |head::tail -> if head = x then acc else findElem(tail)(x)(acc+1)
   





    
    