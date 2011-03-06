module PrimitivesTest

let (!!) str = List.ofSeq str

open NUnit.Framework
open FsUnit

open Cashel

[<Test>]
let test_token () =
    let exp = Some('t', !!"est")
    token !!"test" |> should equal exp
    
[<Test>]
let test_token_empty_list () =
    token [] |> should equal None

[<Test>]
let test_token_single_token () =
    let exp = Some('t', [])
    token ['t'] |> should equal exp

[<Test>]
let test_ignore_with_token () =
    let exp = Some((), !!"est")
    forget token !!"test" |> should equal exp 

[<Test>]
let test_listify_with_token () =
    let exp = Some(['t'], !!"est")
    listify token !!"test" |> should equal exp   
    
[<Test>]
let test_satisy_simple_predicate () =
    let exp = Some('t', !!"est")
    satisfy token (fun x -> x = 't') !!"test" |> should equal exp

[<Test>]
let test_satisy_failure_predicate () =
    satisfy token (fun x -> x = 'e') !!"test" |> should equal None
    

open Cashel.ListPrimitives

[<Test>]
let test_any_success_predicate () =
    any ['q'..'v'] !!"test" |> should equal (Some('t', !!"est"))
    
[<Test>]
let test_any_failure_predicate () =
    any ['a'..'e'] !!"test" |> should equal None
    
[<Test>]
let test_matchToken () =
    matchToken 't' !!"test" |> should equal (Some('t', !!"est"))

[<Test>]
let test_matchToken_failure () =
    matchToken 'e' !!"test" |> should equal None
    
[<Test>]
let test_matchTokens () =
    matchTokens !!"test" !!"test me" |> should equal (Some(!!"test", !!" me"))

[<Test>]
let test_matchTokens_failue () =
    matchTokens !!"test" !!"tesp me" |> should equal None
    
[<Test>]
let test_eof () = 
    eof [] |> should equal (Some((),[]))
    
[<Test>]
let test_eof_fails_not_at_end () = 
    eof !!"test" |> should equal None
    
[<Test>]
let test_repeat () =
    repeat (matchToken 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))

[<Test>]
let test_repeat_one_match() =
    repeat (matchToken 't') !!"test" |> should equal (Some(['t'], !!"est"))
    
[<Test>]
let test_repeat_no_matches () =
    repeat (matchToken 'e') !!"ttttest" |> should equal (Some([], !!"ttttest"))

[<Test>]
let test_repeat1 () =
    repeat1 (matchToken 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))
    
[<Test>]
let test_repeat1_one_match() =
    repeat1 (matchToken 't') !!"test" |> should equal (Some(['t'], !!"est"))

[<Test>]
let test_repeat1_no_matches () =
    repeat1 (matchToken 'e') !!"ttttest" |> should equal None
    
[<Test>]
let test_failure_predicate_parser_success() =
    !~ (matchToken 't') !!"test" |> should equal None

[<Test>]
let test_failure_predicate_parser_fails () =
    !~ (matchToken 'e') !!"test" |> should equal (Some((), !!"test"))
    
[<Test>]
let test_Success_predicate_parser_success() =
    !& (matchToken 't') !!"test" |> should equal (Some((), !!"test"))

[<Test>]
let test_success_predicate_parser_fails () =
    !& (matchToken 'e') !!"test" |> should equal None
    
[<Test>]
let test_option_predicate_one () =
    !? (matchToken 't') !!"test" |> should equal (Some(Some('t'), !!"est"))

[<Test>]
let test_option_predicate_zero () =
    !? (matchToken 'e') !!"test" |> should equal (Some(None, !!"test"))

[<Test>]
let test_ignore_left () =
    ((matchToken 't') .>> (matchToken 'e')) !!"test" |> should equal (Some('t', !!"st"))

[<Test>]
let test_ignore_left_fails () =
    ((matchToken 'e') .>> (matchToken 'e')) !!"test" |> should equal None

[<Test>]
let test_ignore_right () =
    ((matchToken 't') >>. (matchToken 'e')) !!"test" |> should equal (Some('e', !!"st"))

[<Test>]
let test_ignore_right_fails () =
    ((matchToken 't') >>. (matchToken 's')) !!"test" |> should equal None

[<Test>]
let test_parse_return_value () =
    ((matchToken 't') >>! "hello") !!"test" |> should equal (Some("hello", !!"est"))

[<Test>]
let test_parse_return_value_fails () =
    ((matchToken 'q') >>! "hello") !!"test" |> should equal None
    
[<Test>]
let test_until () =
    (until token (matchToken 's')) !!"test" |> should equal (Some(!!"te", !!"t"))
    
[<Test>]
let test_until_fail_1 () =
    (until token (matchToken 'q')) !!"test" |> should equal None

[<Test>]
let test_until_fail_2 () =
    (until (matchToken 'q') (matchToken 's')) !!"test" |> should equal None
    
[<Test>]
let test_skip () =
    let exp = Some((), !!"est")
    skip 't' !!"test" |> should equal exp

[<Test>]
let test_skip_fail () =
    skip 'e' !!"test" |> should equal None
    
[<Test>]
let test_skips () =
    let exp = Some((), !!"st")
    skips !!"te" !!"test" |> should equal exp

[<Test>]
let test_skips_fail () =
    skips !!"ts" !!"test" |> should equal None
    

open Cashel.CharListPrimitives

[<Test>]
let test_space () =
    let exp = Some (' ', !!" test")
    space !!"  test" |> should equal exp
    
[<Test>]
let test_tab () =
    let exp = Some ('\t', !!" test")
    space !!"\t test" |> should equal exp
    
