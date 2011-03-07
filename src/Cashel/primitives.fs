namespace Cashel

[<AutoOpen>]
module Primitives =
    open Cashel

    //-------------------------Basic primitives----------------------------------------------------
    //These primitives make no assumption as to the basic types of the parser input or result types

    ///Custom bind operator >>! binds parser p to result v, ignoring the return value of p
    let (>>!) p v = p >>= (fun _ -> result v)
    
    ///Custom bind operator .>> binds p1 to p2, then returns the parse value of p1
    let (.>>) p1 p2 = p1 >>= (fun v -> p2 >>= (fun _ -> result v))
        
    ///Custom bind operator .>> binds p1 to p2, then returns the parse value of p2
    let (>>.) p1 p2 = p1 >>= (fun _ -> p2 >>= (fun v -> result v))
    
    ///forget tosses the result of parsing function p 
    let forget p = p >>! ()
    
    ///listify turns the result of parsing function p into a single item list
    let listify p = p >>= (fun x -> result [x])
    
    ///repeat looks for zero or more instances of the parsing function p
    let rec repeat p = (repeat1 p) +++ (result [])
    
    ///repeat1 looks for one or more instances of the parsing function p
    and repeat1 p = p >>= (fun x -> repeat p >>= (fun xs -> result (x::xs)))
    
    ///repeat1While looks for one or more instances of the parsing function p while the result of f is true
    and repeat1While f p = p >>= (fun x -> repeat p >>= (fun xs -> if f xs then result (x::xs) else zero))

    ///Success Predicate    
    let (!&) f =        
        fun input ->
            match f input with
            | Some(_) -> Some((),input)
            | None -> None
    
    ///Failure Predicate
    let (!~) f = 
        fun input ->
            match f input with
            | None -> Some((),input)
            | Some(_) -> None
            
    ///Option Predicate
    let (!?) f = 
        fun input ->
            match f input with
            | Some(v,input') -> Some(Some(v),input')
            | None -> Some(None, input)
    
    ///until calls p1 repeatedly until p2 succeeds
    let until p1 p2 =  repeat (!~ p2 >>. p1) .>> p2 
        