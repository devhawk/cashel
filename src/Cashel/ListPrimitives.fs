namespace Cashel

[<AutoOpen>]
module ListPrimitives =
    open Cashel

    //-------------------------List primitives-------------------------------------------
    ///token assumes the input is a list and returns a tuple of the head and tail
    let token : Parser<'a list, 'a> = 
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
            
    ///any checks the value at the start of the input is in the list of tokens l
    let any l = satisfy token (fun x -> l |> List.exists (fun y -> x = y))
    
    ///matchToken checks the value at the start of the input matches the value v
    let matchToken v = satisfy token (fun x -> x = v)
    
    ///matchTokens recursively uses matchToken to check to see if a list of values l matches the start of the input
    let rec matchTokens l = 
        match l with
        | [] -> result []
        | x::xs -> matchToken x >>= (fun i -> matchTokens xs >>= (fun is -> result (i::is)))
    
    ///skip calls matchToken but tosses the parse value
    let skip v = matchToken v |> forget
    
    ///skips calls matchTokens but tosses the parse value
    let skips l = matchTokens l |> forget
