#light

namespace DevHawk.Parser

module Primitives

open DevHawk.Parser.Core

///Custom bind operator >>$ binds parser p to result v, ignoring the return value of p
let (>>$) p v = p >>= (fun _ -> result v)

///Custom bind operator .>> binds p1 to p2, then returns the parse value of p1
let (.>>) p1 p2 = p1 >>= (fun v -> p2 >>= (fun _ -> result v))
    
///Custom bind operator .>> binds p1 to p2, then returns the parse value of p2
let (>>.) p1 p2 = p1 >>= (fun _ -> p2 >>= (fun v -> result v))

///ignore tosses the result of parsing function p 
let ignore p = p >>$ ()

///listify turns the result of parsing function p into a single item list
let listify p = p >>= (fun x -> result [x])

///satisfy checks the value returned from item against the predicate function p
let satisfy parser pred =  parser >>= (fun x -> if pred x then result x else zero)
    
///repeat looks for zero or more instances of the parsing function p
let rec repeat p = (repeat1 p) +++ (result [])

///repeat1 looks for one or more instances of the parsing function p
and repeat1 p = p >>= (fun x -> repeat p >>= (fun xs -> result (x::xs)))

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

///repeat_until calls p1 repeatedly until p2 succeeds
let repeat_until p1 p2 =  repeat (!~ p2 >>. p1) .>> p2 
    

//-------------------------List primitives-------------------------------------------
///item assumes the input is a list and returns a tuple of the head and tail
let item = 
    fun input ->
        match input with
        | x::xs -> Some(x,xs) 
        | [] -> None
        
///eof checks that we're at the end of the list being parsed
let eof = 
    fun input ->
        match input with
        | [] -> Some((), []) 
        | _ -> None
        
///any_of checks the value at the start of the input is in the list of items l
let any_of l = satisfy item (fun x -> l |> List.exists (fun y -> x = y))

///item_equal checks the value at the start of the input matches the value v
let item_equal v = satisfy item (fun x -> x = v)

///items_equal recursively uses item_equal to check to see if a list of values l matches the start of the input
let rec items_equal l = 
    match l with
    | [] -> result []
    | x::xs -> item_equal x >>= (fun i -> items_equal xs >>= (fun is -> result (i::is)))

///skip_item calls item_equal but tosses the parse value
let skip_item v = item_equal v |> ignore

///skip_items calls items_equal but tosses the parse value
let skip_items l = items_equal l |> ignore

