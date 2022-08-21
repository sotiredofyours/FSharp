module FSharp.hw2.fourthTask

open NUnit.Framework
open FsUnit

    (*Реализовать функцию, генерирующую бесконечную последовательность простых чисел.*)
let isPrime n =
    let maxDiv = sqrt (float n)
    let rec check i =
        i > (int maxDiv) || (n % i <> 0 && check (i + 1))
    check 2

let infSeqOfPrime =
    Seq.initInfinite (fun index -> if index <3  then index+1 else index+1) |> 
                                   Seq.filter(fun x -> isPrime x)
        
[<Test>]
let ``PrimeNumbers from zero to nine`` () =
    infSeqOfPrime |> Seq.takeWhile (fun x -> x<9) |> Seq.toList |> should equal [1;2;3;5;7] 