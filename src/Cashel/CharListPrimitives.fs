namespace Cashel
module CharListPrimitives =
    open Cashel

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
    
    let lowercase = any ['a'..'z']
    let uppercase = any ['A'..'Z']
    let letter = lowercase +++ uppercase
    let digit = any ['0'..'9']
    let digitval = digit >>= (fun d -> result (int d - int '0'))
    let space = satisfy token (System.Char.IsWhiteSpace)
    let skipSpace = repeat space |> forget
