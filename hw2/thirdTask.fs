module FSharp.hw2.thirdTask

open NUnit.Framework
open FsUnit

   (*Посчитать значение дерева разбора арифметического выражения, заданного через 
   вложенные discriminated union-ы *)

type TreeExpression =
| Addition of TreeExpression * TreeExpression
| Subtraction of TreeExpression * TreeExpression
| Multiplication of TreeExpression * TreeExpression
| Division of TreeExpression * TreeExpression
| Operand of int

let  rec parseArithmeticTree (tree:TreeExpression) =
    match tree with
    | Addition(p1,p2) -> parseArithmeticTree p1 + parseArithmeticTree p2
    | Subtraction(p1,p2) -> parseArithmeticTree p1 - parseArithmeticTree p2
    | Multiplication(p1,p2) -> parseArithmeticTree p1 * parseArithmeticTree p2
    | Division(p1,p2) -> parseArithmeticTree p1 / parseArithmeticTree p2
    | Operand(p1) -> p1

[<Test>]
let ``test simple tree``() =
    let tree = Addition(Operand 2, Operand 3)
    parseArithmeticTree tree |> should equal 5