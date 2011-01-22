namespace Cashel
module ArraySegmentPrimitives =
  open System
  open Parser

  //-------------------------ArraySegment helpers----------------------------------------------

  let slice (input:ArraySegment<_>) =
    match input with
    | x when x.Count = 0 -> None
    | x -> Some(x.Array.[x.Offset], ArraySegment<'a>(x.Array, x.Offset + 1, x.Count - 1))
    
  //-------------------------ArraySegment primitives-------------------------------------------

  ///token assumes the input is a list and returns a tuple of the head and tail
  let token : Parser<ArraySegment<'a>, 'a> = slice
            
  ///eof checks that we're at the end of the list being parsed
  let eof : Parser<ArraySegment<'a>, unit> = fun input ->
    match input with
    | x when x.Count = 0 -> Some((), x) 
    | _ -> None
            
  ///any checks the value at the start of the input is in the list of items l
  let any l = filter token (fun x -> l |> List.exists (fun y -> x = y))
    
  ///item_equal checks the value at the start of the input matches the value v
  let matchToken v = filter token (fun x -> x = v)
    
  ///items_equal recursively uses item_equal to check to see if a list of values l matches the start of the input
  let rec matchTokens l = 
    match l with
    | [] -> result []
    | x::xs -> matchToken x >>= (fun i -> matchTokens xs >>= (fun is -> result (i::is)))
    
  ///skip_item calls item_equal but tosses the parse value
  let skip v = matchToken v |> forget
    
  ///skip_items calls items_equal but tosses the parse value
  let skips l = matchTokens l |> forget

  ///listify turns the result of parsing function p into a single item list
  let listify p = p >>= (fun x -> result [x])
