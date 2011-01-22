module CharParserPrimitives

let (!!) str = List.ofSeq str

open Cashel.Parser
open Cashel.ListPrimitives
open Cashel.CharListPrimitives
open NUnit.Framework
open BaseSpecs

[<Test>]
let test_space () =
    let exp = Some (' ', !!" test")
    space !!"  test" === exp
    
[<Test>]
let test_tab () =
    let exp = Some ('\t', !!" test")
    space !!"\t test" === exp
