module FSharp.hw2.Program
open FsCheck
open FsUnit
open NUnit.Framework

(* Реализовать три варианта функции, подсчитывающей количество чётных чисел в списке (с использованием стандартных функций map, filter, fold). 
      Использование рекурсии не допускается, зато нужен FsCheck для проверки функций на эквивалентность. *)
let evenNumbers list =
  (List.filter( fun x -> x % 2 =0) list) |> List.map(fun x -> if (x>=0) then (x+1)%2 else -(x+1)%2) |> List.sum
  
let evenNumbersTwo list =
    list |> List.fold(fun s x -> if (x % 2 = 0) then s + 1 else s) 0
    
let evenNumbersThree list =
   match list with
   | _   -> list |> List.map(fun x -> if (x>=0) then (x+1)%2 else -(x+1)%2) |> List.sum 

let checkEqual list = evenNumbersTwo list = evenNumbersThree list && evenNumbers list = evenNumbersTwo list

[<Test>]
let ``list [1,2,3] should return 1``() =
    
    let x = [-2]
    evenNumbers x |> should equal 1
    evenNumbersTwo x |> should equal 1
    evenNumbersThree x |> should equal 1
    Check.QuickThrowOnFailure checkEqual

