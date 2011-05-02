module ParserTest

open NUnit.Framework
open FsUnit

open Cashel

let (!!) str = List.ofSeq str
let hello = !!"hello"

[<Test>]
let test_result () = 
    result 1 "hello" |> should equal (Some(1,"hello"))
    
[<Test>]
let test_zero () =
    zero "hello" |> should equal None
    
[<Test>]
let test_bind () =
    let p1 = fun input -> Some(2,input)
    let p2 x = fun input -> Some(5*x,input)
    let p3 = p1 >>= p2
    p3 "hello" |> should equal (Some(10, "hello"))
    
[<Test>]
let test_combine_both_work () =
    let p1 = fun input -> Some(1,input)
    let p2 = fun input -> Some(2,input) 
    let p3 = p1 +++ p2
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Test>]
let test_combine_second_Fails () =
    let p1 = fun input -> Some(1,input)
    let p3 = p1 +++ zero
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Test>]
let test_combine_first_fails () =
    let p1 = fun input -> Some(1,input)
    let p3 = zero +++ p1 
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Test>]
let test_combine_both_fail () =
    let p3 = zero +++ zero
    p3 "hello" |> should equal None
    
[<Test>]
let test_monad_zero () =
    let p = parser { if false then return 't' }
    p "test" |> should equal None
    

