module FSharp.hw2.Program
open FsUnit
open NUnit.Framework

(* Реализовать три варианта функции, подсчитывающей количество чётных чисел в списке (с использованием стандартных функций map, filter, fold). 
      Использование рекурсии не допускается, зато нужен FsCheck для проверки функций на эквивалентность. *)
let rec evenNumbers list =
  (List.filter( fun x -> x % 2 =0) list).Length 

let evenNumbersTwo list =
    list |> List.fold(fun s x -> if (x % 2 = 0) then s + 1 else s) 0

let evenNumbersThree list =
   list |> List.map(fun x -> (x+1)%2) |> List.sum
                                         

[<Test>]
let ``list [1,2,3] should return 1``() =
    let x = [1;2;3]
    evenNumbers x |> should equal 1
    evenNumbersTwo x |> should equal 1
    evenNumbersThree x |> should equal 1

