#light

namespace DevHawk.Parser

module Primitives

open DevHawk.Parser.Core

//let (!!) s = List.of_seq s


//item assumes the input is a list and returns a tuple of the head and tail
let item = 
    fun input ->
        match input with
        | x::xs -> Some(x,xs) 
        | [] -> None
        
//ignore tosses the result of parsing function p 
let ignore p = p >>= (fun x -> result ())

//listify turns the result of parsing function p into a single item list
let listify p = p >>= (fun x -> result [x])

//satisfy checks the value returned from item against the predicate function p
let satisfy parser pred = 
    parse {
        let! x = parser
        if pred x then return x }


//any_of checks the value at the start of the input is in the list of items l
let any_of l = satisfy item (fun x -> List.exists (fun y -> x = y) l)

//item_equal checks the value at the start of the input matches the value v
let item_equal v = satisfy item (fun x -> x = v)

//items_equal recursively uses item_equal to check to see if a list of values l matches the start of the input
let rec items_equal l =
    match l with
    | [] -> result []
    | x::xs -> parse { 
        let! i = item_equal x 
        let! is = items_equal xs 
        return i::is }

//eof checks that we're at the end of the list being parsed
let eof = 
    fun input ->
        match input with
        | [] -> Some((), []) 
        | _ -> None

//repeat looks for zero or more instances of the parsing function p
let rec repeat p = 
    parse { return! repeat1 p
            return [] }

//repeat looks for one or more instances of the parsing function p
and repeat1 p =
    parse { let! x = p
            let! xs = repeat p
            return x::xs }

    

//Success Predicate    
let (!&) f =        
    fun input ->
        match f input with
        | Some(_) -> Some((),input)
        | None -> None

//Failure Predicate
let (!~) f = 
    fun input ->
        match f input with
        | None -> Some((),input)
        | Some(_) -> None
        
//Option Predicate
let (!?) f = 
    fun input ->
        match f input with
        | Some(v,input') -> Some(Some(v),input')
        | None -> Some(None, input)
        
//.>> parses both p1 and p2, but only returns the value of p1
let (.>>) p1 p2 = parse {
    let! x = p1
    do! p2 |> ignore 
    return x }
    
//.>> parses both p1 and p2, but only returns the value of p2
let (>>.) p1 p2 = parse {
    do! p1 |> ignore 
    let! x = p2
    return x }

