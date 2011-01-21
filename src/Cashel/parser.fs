namespace Cashel

[<AutoOpen>]
module Core =
  open System
              
  type Parser<'input, 'result> = 'input -> ('result * 'input) option

  //I define the monadic operators (zero, result, bind & choice) as individual 
  //functions so I can use them outside the parse monad
  let result v : Parser<'i,'r> = fun input -> Some(v, input)

  let bind (p: Parser<'i,'a>) (f: 'a -> Parser<'i,'b>) : Parser<'i,'b> =
    fun input -> 
      match p input with
      | Some(value, input) -> f value input
      | None -> None

  let zero : Parser<'i,'r> = fun input -> None
    
  let choice (p: Parser<'i,'a>) (q: Parser<'i,'a>) : Parser<'i,'a> =
    fun input ->
      match p input with
      | Some(v) -> Some(v)
      | None -> q input

  //I define infix operator versions of bind and choice to make it more 
  //convenient to call
  let (>>=) = bind
  let (+++) = choice

  type ParserBuilder() =
    member x.Delay(f) = fun input -> f () input
    member x.Zero() = zero
    member x.Return(v) = result v
    member x.ReturnFrom(p) = p
    member x.Yield(v) = result v
    member x.YieldFrom(p) = p
    member x.Bind(p, f) = p >>= f
    member x.Combine(p, q) = p +++ q

    member this.TryWith(p, h) = fun input -> 
      try p input
      with e -> h e input

    member this.TryFinally(p, compensation) = fun input ->
      try p input
      finally compensation()

    member this.Using(res:#IDisposable, body) =
      this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))

    member this.While(guard, m) =
      if not(guard()) then zero else
        m >>= (fun () -> this.While(guard, m))

    member this.For(sequence:seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                 (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

  let parse = ParserBuilder()

module Parser =
  let map p f = parse {
    let! x = p
    return f x }

  let filter p f = parse {
    let! x = p
    if f x then return x
    else return! zero }

  let unfold seed f next =
    let rec loop curr = parse {
      if f curr then return curr
      else return! zero
      return! loop (next curr) }
    loop seed

  let (<*>) f a = parse {
    let! f' = f
    let! a' = a
    return f' a' }