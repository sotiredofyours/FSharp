module FSharp.hw2.secondTask
open FsUnit
open NUnit.Framework

(*2. Реализовать функцию, применяющую функцию к каждому элементу двоичного дерева и возвращающую новое двоичное дерево, 
    каждый элемент которого — результат применения функции к соответствующему элементу исходного дерева (map для деревьев).*)
    
type Tree =
    | Tip
    | Node of int * Tree * Tree
let rec mapTree func tree =
   match tree with
      | Node(value, left, right) -> Node (func value, mapTree func left, mapTree func right)                                                     
      | Tip -> Tip
     
[<Test>]
let ``testTree``() =
    let myTree = Node(2, Node(1, Tip, Tip), Node(3, Tip, Tip))
    let square x = x * x
    let result = Node(4, Node(1, Tip, Tip), Node(9, Tip, Tip))
    mapTree square myTree |> should equal result