// Turn on the lightweight syntax
#light

namespace DevHawk.Parser

module Core
          
type Parser<'input, 'result> = 'input -> ('result * 'input) option

//I define the monadic operators (zero, result, bind & choice) as individual 
//functions so I can use them outside the parse monad
let result v : Parser<'i,'r> = fun input -> Some(v, input)

let bind p f : Parser<'i,'r> = 
    fun input ->
        match p input with
        | Some(value, input) -> f value input
        | None -> None

let zero : Parser<'i,'r> = fun input -> None

let choice p q : Parser<'i,'r> = 
    fun input ->
        match p input with
        | Some(v) -> Some(v)
        | None -> q input

//I define infix operator versions of bind and choice to make it more 
//convienent to call
let (>>=) = bind
let (+++) = choice

//Here's the parser monad definition
type ParserBuilder() =
    member w.Delay(f) = fun input -> f () input 
    member w.Zero() = zero
    member w.Return(v) = result v 
    member w.Bind(p, f) = p >>= f
    member w.Combine(p1,p2) = p1 +++ p2
    
let parser = ParserBuilder()



