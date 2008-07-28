// Turn on the lightweight syntax
#light

open Xunit
open FsxUnit.Syntax

open DevHawk.Parser.Core

let (!!) str = List.of_seq str
let hello = !!"hello"

[<Fact>]
let test_result () = 
    result 1 "hello" |> should equal (Some(1,"hello"))
    
[<Fact>]
let test_zero () =
    zero "hello" |> should equal None
    
[<Fact>]
let test_bind () =
    let p1 = fun input -> Some(2,input)
    let p2 x = fun input -> Some(5*x,input)
    let p3 = p1 >>= p2
    p3 "hello" |> should equal (Some(10, "hello"))
    
[<Fact>]
let test_combine_both_work () =
    let p1 = fun input -> Some(1,input)
    let p2 = fun input -> Some(2,input) 
    let p3 = p1 +++ p2
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Fact>]
let test_combine_second_Fails () =
    let p1 = fun input -> Some(1,input)
    let p3 = p1 +++ zero
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Fact>]
let test_combine_first_fails () =
    let p1 = fun input -> Some(1,input)
    let p3 = zero +++ p1 
    p3 "hello" |> should equal (Some(1, "hello"))
    
[<Fact>]
let test_combine_both_fail () =
    let p3 = zero +++ zero
    p3 "hello" |> should equal None
    

//result a >>= f == f a
//p >>= result == p
//p >>= (fun a -> (f a >>= g)) == (p >>= (fun a -> f a)) >>= g


    

(*[<Fact>]
let test_str () = 
    token !!"test" !!"testing" |> should equal (Some((), !!"ing"))*)
