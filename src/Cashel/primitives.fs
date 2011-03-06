namespace DevHawk.Parser

module Primitives =
    
    open DevHawk.Parser.Core
    
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
    
    ///repeatUntil calls p1 repeatedly until p2 succeeds
    let repeatUntil p1 p2 =  repeat (!~ p2 >>. p1) .>> p2 
        
    
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
    
    
    //-------------------------char list primitives-------------------------------------------
    
    let addLineAndCol cl =
        let rec worker cl line col =
            match cl with
            | '\r'::'\n'::tail -> ('\r', line, col)::('\n', line, (col+1))::(worker tail (line+1) 1)
            | '\n'::tail -> ('\n', line, col)::(worker tail (line+1) 1)
            | '\r'::tail -> ('\r', line, col)::(worker tail (line+1) 1)
            | head::tail -> (head, line, col)::(worker tail line (col+1))
            | [] -> []
        worker cl 1 1 
    
    let lowercase = anyOf ['a'..'z']
    let uppercase = anyOf ['A'..'Z']
    let letter = lowercase +++ uppercase
    let digit = anyOf ['0'..'9']
    let digitval = digit >>= (fun d -> result (int d - int '0'))
    let space = satisfy item (System.Char.IsWhiteSpace)
    let skipSpace = repeat space |> forget