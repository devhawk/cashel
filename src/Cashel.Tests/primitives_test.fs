module PrimitivesTest

let (!!) str = List.ofSeq str

open NUnit.Framework
open FsUnit

open DevHawk.Parser.Primitives

[<Test>]
let test_item () =
    let exp = Some('t', !!"est")
    item !!"test" |> should equal exp
    
[<Test>]
let test_item_empty_list () =
    item [] |> should equal None

[<Test>]
let test_item_single_item () =
    let exp = Some('t', [])
    item ['t'] |> should equal exp

[<Test>]
let test_ignore_with_item () =
    let exp = Some((), !!"est")
    forget item !!"test" |> should equal exp 

[<Test>]
let test_listify_with_item () =
    let exp = Some(['t'], !!"est")
    listify item !!"test" |> should equal exp   
    
[<Test>]
let test_satisy_simple_predicate () =
    let exp = Some('t', !!"est")
    satisfy item (fun x -> x = 't') !!"test" |> should equal exp

[<Test>]
let test_satisy_failure_predicate () =
    satisfy item (fun x -> x = 'e') !!"test" |> should equal None
    
[<Test>]
let test_anyOf_success_predicate () =
    anyOf ['q'..'v'] !!"test" |> should equal (Some('t', !!"est"))
    
[<Test>]
let test_anyOf_failure_predicate () =
    anyOf ['a'..'e'] !!"test" |> should equal None
    
[<Test>]
let test_itemEqual () =
    itemEqual 't' !!"test" |> should equal (Some('t', !!"est"))

[<Test>]
let test_itemEqual_failure () =
    itemEqual 'e' !!"test" |> should equal None
    
[<Test>]
let test_itemsEqual () =
    itemsEqual !!"test" !!"test me" |> should equal (Some(!!"test", !!" me"))

[<Test>]
let test_itemsEqual_failue () =
    itemsEqual !!"test" !!"tesp me" |> should equal None
    
[<Test>]
let test_eof () = 
    eof [] |> should equal (Some((),[]))
    
[<Test>]
let test_eof_fails_not_at_end () = 
    eof !!"test" |> should equal None
    
[<Test>]
let test_repeat () =
    repeat (itemEqual 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))

[<Test>]
let test_repeat_one_match() =
    repeat (itemEqual 't') !!"test" |> should equal (Some(['t'], !!"est"))
    
[<Test>]
let test_repeat_no_matches () =
    repeat (itemEqual 'e') !!"ttttest" |> should equal (Some([], !!"ttttest"))

[<Test>]
let test_repeat1 () =
    repeat1 (itemEqual 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))
    
[<Test>]
let test_repeat1_one_match() =
    repeat1 (itemEqual 't') !!"test" |> should equal (Some(['t'], !!"est"))

[<Test>]
let test_repeat1_no_matches () =
    repeat1 (itemEqual 'e') !!"ttttest" |> should equal None
    
[<Test>]
let test_failure_predicate_parser_success() =
    !~ (itemEqual 't') !!"test" |> should equal None

[<Test>]
let test_failure_predicate_parser_fails () =
    !~ (itemEqual 'e') !!"test" |> should equal (Some((), !!"test"))
    
[<Test>]
let test_Success_predicate_parser_success() =
    !& (itemEqual 't') !!"test" |> should equal (Some((), !!"test"))

[<Test>]
let test_success_predicate_parser_fails () =
    !& (itemEqual 'e') !!"test" |> should equal None
    
[<Test>]
let test_option_predicate_one () =
    !? (itemEqual 't') !!"test" |> should equal (Some(Some('t'), !!"est"))

[<Test>]
let test_option_predicate_zero () =
    !? (itemEqual 'e') !!"test" |> should equal (Some(None, !!"test"))

[<Test>]
let test_ignore_left () =
    ((itemEqual 't') .>> (itemEqual 'e')) !!"test" |> should equal (Some('t', !!"st"))

[<Test>]
let test_ignore_left_fails () =
    ((itemEqual 'e') .>> (itemEqual 'e')) !!"test" |> should equal None

[<Test>]
let test_ignore_right () =
    ((itemEqual 't') >>. (itemEqual 'e')) !!"test" |> should equal (Some('e', !!"st"))

[<Test>]
let test_ignore_right_fails () =
    ((itemEqual 't') >>. (itemEqual 's')) !!"test" |> should equal None

[<Test>]
let test_parse_return_value () =
    ((itemEqual 't') >>! "hello") !!"test" |> should equal (Some("hello", !!"est"))

[<Test>]
let test_parse_return_value_fails () =
    ((itemEqual 'q') >>! "hello") !!"test" |> should equal None
    
[<Test>]
let test_repeatUntil () =
    (repeatUntil item (itemEqual 's')) !!"test" |> should equal (Some(!!"te", !!"t"))
    
[<Test>]
let test_repeatUntil_fail_1 () =
    (repeatUntil item (itemEqual 'q')) !!"test" |> should equal None

[<Test>]
let test_repeatUntil_fail_2 () =
    (repeatUntil (itemEqual 'q') (itemEqual 's')) !!"test" |> should equal None
    
[<Test>]
let test_skipItem () =
    let exp = Some((), !!"est")
    skipItem 't' !!"test" |> should equal exp

[<Test>]
let test_skipItem_fail () =
    skipItem 'e' !!"test" |> should equal None
    
[<Test>]
let test_skipItems () =
    let exp = Some((), !!"st")
    skipItems !!"te" !!"test" |> should equal exp

[<Test>]
let test_skipItems_fail () =
    skipItems !!"ts" !!"test" |> should equal None
    
[<Test>]
let test_space () =
    let exp = Some (' ', !!" test")
    space !!"  test" |> should equal exp
    
[<Test>]
let test_tab () =
    let exp = Some ('\t', !!" test")
    space !!"\t test" |> should equal exp
    
