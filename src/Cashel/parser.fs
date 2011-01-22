namespace Cashel
module Parser =
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

  //-------------------------Basic primitives----------------------------------------------------
  //These primitives make no assumption as to the basic types of the parser input or result types

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
   
  ///Custom bind operator >>! binds parser p to result v, ignoring the return value of p
  let (>>!) p v = p >>= (fun _ -> result v)
   
  ///Custom bind operator .>> binds p1 to p2, then returns the parse value of p1
  let (.>>) p1 p2 = p1 >>= (fun v -> p2 >>= (fun _ -> result v))
        
  ///Custom bind operator .>> binds p1 to p2, then returns the parse value of p2
  let (>>.) p1 p2 = p1 >>= (fun _ -> p2 >>= (fun v -> result v))
    
  ///forget tosses the result of parsing function p 
  let forget p = p >>! ()
    
  ///repeat looks for zero or more instances of the parsing function p
  let rec repeat p = (repeat1 p) +++ (result [])
    
  ///repeat1 looks for one or more instances of the parsing function p
  and repeat1 p = p >>= (fun x -> repeat p >>= (fun xs -> result (x::xs)))
    
  ///Success Predicate    
  let (!&) f = fun input ->
    match f input with
    | Some(_) -> Some((),input)
    | None -> None
    
  ///Failure Predicate
  let (!~) f = fun input ->
    match f input with
    | None -> Some((),input)
    | Some(_) -> None
            
  ///Option Predicate
  let (!?) f = fun input ->
    match f input with
    | Some(v,input') -> Some(Some(v),input')
    | None -> Some(None, input)
    
  ///until calls p1 repeatedly until p2 succeeds
  let until p1 p2 =  repeat (!~ p2 >>. p1) .>> p2 
