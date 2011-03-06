module ArraySegmentPrimitivesTest
open System

let (!!) str = List.ofSeq str
let (!!!) str = ArraySegment<_>(str)
let (!!+) str offset = ArraySegment<_>(str, offset, str.Length - offset)
let (!+) str = !!+str 1

open NUnit.Framework
open FsUnit

open Cashel
open Cashel.ArraySegmentPrimitives

let inline (===>) (actual: (_ * ArraySegment<_>) option) (expected: (_ * ArraySegment<_>) option) =
  if actual.IsNone && expected.IsNone then actual |> should equal expected
  else
    let avalue, asegment = actual |> Option.get
    let evalue, esegment = expected |> Option.get
    evalue = avalue |> should be True
    asegment.Array |> should equal esegment.Array
    asegment.Offset |> should equal esegment.Offset
    asegment.Count |> should equal esegment.Count

let test = "test"B
let test_me = "test me"B
let ttttest = "ttttest"B

[<Test>]
let test_token () =
    let exp = Some('t'B, !+test)
    token !!!test ===> exp
    
[<Test>]
let test_token_empty_list () =
    token !!!""B ===> None

[<Test>]
let test_token_single_token () =
    let t = "t"B
    let exp = Some('t'B, !+t)
    token !!!t ===> exp

[<Test>]
let test_ignore_with_token () =
    let exp = Some((), !+test)
    forget token !!!test ===> exp

[<Test>]
let test_listify_with_token () =
    let exp = Some(['t'B], !+test)
    listify token !!!test ===> exp
    
[<Test>]
let test_filter_simple_predicate () =
    let exp = Some('t'B, !+test)
    filter token (fun x -> x = 't'B) !!!test ===> exp

[<Test>]
let test_filter_failure_predicate () =
    filter token (fun x -> x = 'e'B) !!!test ===> None
    
[<Test>]
let test_any_success_predicate () =
    any ['q'B..'v'B] !!!test ===> (Some('t'B, !+test)) 
    
[<Test>]
let test_any_failure_predicate () =
    any ['a'B..'e'B] !!!test ===> None
    
[<Test>]
let test_matchToken () =
    matchToken 't'B !!!test ===> (Some('t'B, !+test)) 

[<Test>]
let test_matchToken_failure () =
    matchToken 'e'B !!!test ===> None
    
[<Test>]
let test_matchTokens () =
    matchTokens !!test !!!test_me ===> (Some(!!test, !!+test_me 4)) 

[<Test>]
let test_matchTokens_failue () =
    matchTokens !!test !!!"tesp me"B ===> None
    
[<Test>]
let test_eof () = 
    eof !!!""B ===> (Some((),!!!""B)) 
    
[<Test>]
let test_eof_fails_not_at_end () = 
    eof !!!test ===> None
    
[<Test>]
let test_repeat () =
    repeat (matchToken 't'B) !!!ttttest ===> (Some(!!"tttt"B, !!+ttttest 4))

[<Test>]
let test_repeat_one_match() =
    repeat (matchToken 't'B) !!!test ===> (Some(['t'B], !+test))
    
[<Test>]
let test_repeat_no_matches () =
    repeat (matchToken 'e'B) !!!ttttest ===> (Some([], !!!ttttest))

[<Test>]
let test_repeat1 () =
    repeat1 (matchToken 't'B) !!!ttttest ===> (Some(!!"tttt"B, !!+ttttest 4))
    
[<Test>]
let test_repeat1_one_match() =
    repeat1 (matchToken 't'B) !!!test ===> (Some(['t'B], !+test))

[<Test>]
let test_repeat1_no_matches () =
    repeat1 (matchToken 'e'B) !!!ttttest ===> None
    
[<Test>]
let test_failure_predicate_parser_success() =
    !~ (matchToken 't'B) !!!test ===> None

[<Test>]
let test_failure_predicate_parser_fails () =
    !~ (matchToken 'e'B) !!!test ===> (Some((), !!!test))
    
[<Test>]
let test_Success_predicate_parser_success() =
    !& (matchToken 't'B) !!!test ===> (Some((), !!!test))

[<Test>]
let test_success_predicate_parser_fails () =
    !& (matchToken 'e'B) !!!test ===> None
    
[<Test>]
let test_option_predicate_one () =
    !? (matchToken 't'B) !!!test ===> (Some(Some('t'B), !+test))

[<Test>]
let test_option_predicate_zero () =
    !? (matchToken 'e'B) !!!test ===> (Some(None, !!!test))

[<Test>]
let test_ignore_left () =
    ((matchToken 't'B) .>> (matchToken 'e'B)) !!!test ===> (Some('t'B, !!+test 2))

[<Test>]
let test_ignore_left_fails () =
    ((matchToken 'e'B) .>> (matchToken 'e'B)) !!!test ===> None

[<Test>]
let test_ignore_right () =
    ((matchToken 't'B) >>. (matchToken 'e'B)) !!!test ===> (Some('e'B, !!+test 2))

[<Test>]
let test_ignore_right_fails () =
    ((matchToken 't'B) >>. (matchToken 's'B)) !!!test ===> None

[<Test>]
let test_parse_return_value () =
    ((matchToken 't'B) >>! !!"hello"B) !!!test ===> (Some(!!"hello"B, !+test))

[<Test>]
let test_parse_return_value_fails () =
    ((matchToken 'q'B) >>! !!"hello"B) !!!test ===> None
    
[<Test>]
let test_until () =
    (until token (matchToken 's'B)) !!!test ===> (Some(!!"te"B, !!+test 3))
    
[<Test>]
let test_until_fail_1 () =
    (until token (matchToken 'q'B)) !!!test ===> None

[<Test>]
let test_until_fail_2 () =
    (until (matchToken 'q'B) (matchToken 's'B)) !!!test ===> None
    
[<Test>]
let test_skip () =
    let exp = Some((), !+test)
    skip 't'B !!!test ===> exp

[<Test>]
let test_skip_fail () =
    skip 'e'B !!!test ===> None
    
[<Test>]
let test_skips () =
    let exp = Some((), !!+test 2)
    skips !!"te"B !!!test ===> exp

[<Test>]
let test_skips_fail () =
    skips !!"ts"B !!!test ===> None
