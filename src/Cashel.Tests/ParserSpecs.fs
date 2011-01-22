module ParserSpecs
open Cashel.Parser
open NUnit.Framework
open BaseSpecs

let (!!) str = List.ofSeq str
let hello = !!"hello"

[<Test>]
let test_result() =
  result 1 "hello" === (Some(1, "hello"))
    
[<Test>]
let test_zero() =
  zero "hello" === None
    
[<Test>]
let test_bind () =
  let p1 = result 2
  let p2 x = result (5*x)
  let p3 = p1 >>= p2
  p3 "hello" === (Some(10, "hello"))
    
[<Test>]
let test_combine_both_work () =
  let p1 = result 1
  let p2 = result 2
  let p3 = p1 +++ p2
  p3 "hello" === (Some(1, "hello"))
    
[<Test>]
let test_combine_second_Fails () =
  let p1 = result 1
  let p3 = p1 +++ zero
  p3 "hello" === (Some(1, "hello"))
    
[<Test>]
let test_combine_first_fails () =
  let p1 = result 1
  let p3 = zero +++ p1 
  p3 "hello" === (Some(1, "hello"))
    
[<Test>]
let test_combine_both_fail () =
  let p3 = zero +++ zero
  p3 "hello" === None
    
[<Test>]
let test_monad_zero () =
  let p = parse { if false then return 't' }
  p "test" === None