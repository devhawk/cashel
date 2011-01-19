namespace Cashel

[<AutoOpen>]
module Core =
  open System
              
  type Parser<'input, 'result> = 'input -> Async<('result * 'input) option>

  //I define the monadic operators (zero, result, bind & choice) as individual 
  //functions so I can use them outside the parse monad
  let result v : Parser<'i,'r> = fun input -> async { return Some(v, input) }

  let bind (p: Parser<'i,'a>) (f: 'a -> Parser<'i,'b>) : Parser<'i,'b> =
    fun input -> async {
      let! comp = p input
      return! match comp with
              | Some(value, input) -> f value input
              | None -> async { return None } }

  let zero : Parser<'i,'r> = fun input -> async { return None }
    
  let choice (p: Parser<'i,'a>) (q: Parser<'i,'a>) : Parser<'i,'a> =
    fun input -> async {
      let! comp = p input
      return! match comp with
              | Some(v) -> async { return Some(v) }
              | None -> q input }

  //I define infix operator versions of bind and choice to make it more 
  //convenient to call
  let (>>=) = bind
  let (+++) = choice

  type ParserBuilder() =
    member x.Delay(f) = fun input -> f () input
    member x.Zero() = zero
    member x.Return(v) = result v
    member x.ReturnFrom(p) : Parser<'i,'r> = p
    member x.Yield(v) = result v
    member x.YieldFrom(p) : Parser<'i,'r> = p
    member x.Bind(p, f) = p >>= f
    member x.Combine(p, q) = p +++ q

    member this.TryWith(p:Parser<'i,'r>, h) : Parser<'i,'r> =
      fun input -> async {
        try return! p input
        with e -> return! h e input }

    member this.TryFinally(p:Parser<'i,'r>, compensation) : Parser<'i,'r> =
      fun input -> async {
        try return! p input
        finally compensation() }

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
  let run input parser = Async.RunSynchronously(parser input)
  let runParallel input parser = parser input |> Async.Parallel |> Async.RunSynchronously
  let start input parser = Async.Start(parser input)
  let continueWith input cont exCont parser = Async.StartWithContinuations(parser input, cont, exCont, exCont)

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