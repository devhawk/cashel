module ListPrimitivesSpecs

let (!!) str = List.ofSeq str

open Cashel.Parser
open Cashel.ListPrimitives
open NUnit.Framework
open BaseSpecs

[<Test>]
let test_token () =
    let exp = Some('t', !!"est")
    token !!"test" === exp
    
[<Test>]
let test_token_empty_list () =
    token [] === None

[<Test>]
let test_token_single_token () =
    let exp = Some('t', [])
    token ['t'] === exp

[<Test>]
let test_ignore_with_token () =
    let exp = Some((), !!"est")
    forget token !!"test" === exp

[<Test>]
let test_listify_with_token () =
    let exp = Some(['t'], !!"est")
    listify token !!"test" === exp
    
[<Test>]
let test_filter_simple_predicate () =
    let exp = Some('t', !!"est")
    filter token (fun x -> x = 't') !!"test" === exp

[<Test>]
let test_filter_failure_predicate () =
    filter token (fun x -> x = 'e') !!"test" === None
    
[<Test>]
let test_any_success_predicate () =
    any ['q'..'v'] !!"test" === (Some('t', !!"est")) 
    
[<Test>]
let test_any_failure_predicate () =
    any ['a'..'e'] !!"test" === None
    
[<Test>]
let test_matchToken () =
    matchToken 't' !!"test" === (Some('t', !!"est")) 

[<Test>]
let test_matchToken_failure () =
    matchToken 'e' !!"test" === None
    
[<Test>]
let test_matchTokens () =
    matchTokens !!"test" !!"test me" === (Some(!!"test", !!" me")) 

[<Test>]
let test_matchTokens_failue () =
    matchTokens !!"test" !!"tesp me" === None
    
[<Test>]
let test_eof () = 
    eof [] === (Some((),[])) 
    
[<Test>]
let test_eof_fails_not_at_end () = 
    eof !!"test" === None
    
[<Test>]
let test_repeat () =
    repeat (matchToken 't') !!"ttttest" === (Some(!!"tttt", !!"est"))

[<Test>]
let test_repeat_one_match() =
    repeat (matchToken 't') !!"test" === (Some(['t'], !!"est"))
    
[<Test>]
let test_repeat_no_matches () =
    repeat (matchToken 'e') !!"ttttest" === (Some([], !!"ttttest"))

[<Test>]
let test_repeat1 () =
    repeat1 (matchToken 't') !!"ttttest" === (Some(!!"tttt", !!"est"))
    
[<Test>]
let test_repeat1_one_match() =
    repeat1 (matchToken 't') !!"test" === (Some(['t'], !!"est"))

[<Test>]
let test_repeat1_no_matches () =
    repeat1 (matchToken 'e') !!"ttttest" === None

[<Test>]
let test_repeat1While () =
    repeat1While (fun xs -> xs.Length < 4) (matchToken 't') !!"ttttest" === (Some(!!"tttt", !!"est"))
    
[<Test>]
let test_repeat1While_one_match() =
    repeat1While (fun xs -> xs.Length < 4) (matchToken 't') !!"test" === (Some(['t'], !!"est"))

[<Test>]
let test_repeat1While_no_matches () =
    repeat1While (fun xs -> xs.Length < 4) (matchToken 'e') !!"ttttest" === None

[<Test>]
let test_repeat1While_too_many_matches() =
    repeat1While (fun xs -> xs.Length < 3) (matchToken 't') !!"ttttest" === None

[<Test>]
let test_failure_predicate_parser_success() =
    !~ (matchToken 't') !!"test" === None

[<Test>]
let test_failure_predicate_parser_fails () =
    !~ (matchToken 'e') !!"test" === (Some((), !!"test"))
    
[<Test>]
let test_Success_predicate_parser_success() =
    !& (matchToken 't') !!"test" === (Some((), !!"test"))

[<Test>]
let test_success_predicate_parser_fails () =
    !& (matchToken 'e') !!"test" === None
    
[<Test>]
let test_option_predicate_one () =
    !? (matchToken 't') !!"test" === (Some(Some('t'), !!"est"))

[<Test>]
let test_option_predicate_zero () =
    !? (matchToken 'e') !!"test" === (Some(None, !!"test"))

[<Test>]
let test_ignore_left () =
    ((matchToken 't') .>> (matchToken 'e')) !!"test" === (Some('t', !!"st"))

[<Test>]
let test_ignore_left_fails () =
    ((matchToken 'e') .>> (matchToken 'e')) !!"test" === None

[<Test>]
let test_ignore_right () =
    ((matchToken 't') >>. (matchToken 'e')) !!"test" === (Some('e', !!"st"))

[<Test>]
let test_ignore_right_fails () =
    ((matchToken 't') >>. (matchToken 's')) !!"test" === None

[<Test>]
let test_parse_return_value () =
    ((matchToken 't') >>! "hello") !!"test" === (Some("hello", !!"est"))

[<Test>]
let test_parse_return_value_fails () =
    ((matchToken 'q') >>! "hello") !!"test" === None
    
[<Test>]
let test_until () =
    (until token (matchToken 's')) !!"test" === (Some(!!"te", !!"t"))
    
[<Test>]
let test_until_fail_1 () =
    (until token (matchToken 'q')) !!"test" === None

[<Test>]
let test_until_fail_2 () =
    (until (matchToken 'q') (matchToken 's')) !!"test" === None
    
[<Test>]
let test_skip () =
    let exp = Some((), !!"est")
    skip 't' !!"test" === exp

[<Test>]
let test_skip_fail () =
    skip 'e' !!"test" === None
    
[<Test>]
let test_skips () =
    let exp = Some((), !!"st")
    skips !!"te" !!"test" === exp

[<Test>]
let test_skips_fail () =
    skips !!"ts" !!"test" === None