namespace Cashel

[<AutoOpen>]
module ListPrimitives =
    open Cashel

    //-------------------------List primitives-------------------------------------------
    ///item assumes the input is a list and returns a tuple of the head and tail
    let item : Parser<'a list, 'a> = 
        fun input ->
            match input with
            | x::xs -> Some(x,xs) 
            | [] -> None
            
    ///eof checks that we're at the end of the list being parsed
    let eof : Parser<'a list, unit> = 
        fun input ->
            match input with
            | [] -> Some((), []) 
            | _ -> None
            
    ///anyOf checks the value at the start of the input is in the list of items l
    let anyOf l = satisfy item (fun x -> l |> List.exists (fun y -> x = y))
    
    ///itemEqual checks the value at the start of the input matches the value v
    let itemEqual v = satisfy item (fun x -> x = v)
    
    ///itemsEqual recursively uses itemEqual to check to see if a list of values l matches the start of the input
    let rec itemsEqual l = 
        match l with
        | [] -> result []
        | x::xs -> itemEqual x >>= (fun i -> itemsEqual xs >>= (fun is -> result (i::is)))
    
    ///skipItem calls itemEqual but tosses the parse value
    let skipItem v = itemEqual v |> forget
    
    ///skipItems calls itemsEqual but tosses the parse value
    let skipItems l = itemsEqual l |> forget
