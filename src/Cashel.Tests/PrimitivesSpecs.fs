module PrimitivesSpecs

let (!!) str = List.ofSeq str

open Cashel
open NUnit.Framework
open BaseSpecs

[<Test>]
let test_item () =
    let exp = Some('t', !!"est")
    item !!"test" == exp
    
[<Test>]
let test_item_empty_list () =
    item [] == None

[<Test>]
let test_item_single_item () =
    let exp = Some('t', [])
    item ['t'] == exp

[<Test>]
let test_ignore_with_item () =
    let exp = Some((), !!"est")
    forget item !!"test" == exp

[<Test>]
let test_listify_with_item () =
    let exp = Some(['t'], !!"est")
    listify item !!"test" == exp
    
[<Test>]
let test_satisy_simple_predicate () =
    let exp = Some('t', !!"est")
    satisfy item (fun x -> x = 't') !!"test" == exp

[<Test>]
let test_satisy_failure_predicate () =
    satisfy item (fun x -> x = 'e') !!"test" == None
    
[<Test>]
let test_any_of_success_predicate () =
    any_of ['q'..'v'] !!"test" == (Some('t', !!"est")) 
    
[<Test>]
let test_any_of_failure_predicate () =
    any_of ['a'..'e'] !!"test" == None
    
[<Test>]
let test_item_equal () =
    item_equal 't' !!"test" == (Some('t', !!"est")) 

[<Test>]
let test_item_equal_failure () =
    item_equal 'e' !!"test" == None
    
[<Test>]
let test_items_equal () =
    items_equal !!"test" !!"test me" == (Some(!!"test", !!" me")) 

[<Test>]
let test_items_equal_failue () =
    items_equal !!"test" !!"tesp me" == None
    
[<Test>]
let test_eof () = 
    eof [] == (Some((),[])) 
    
[<Test>]
let test_eof_fails_not_at_end () = 
    eof !!"test" == None
    
[<Test>]
let test_repeat () =
    repeat (item_equal 't') !!"ttttest" == (Some(!!"tttt", !!"est"))

[<Test>]
let test_repeat_one_match() =
    repeat (item_equal 't') !!"test" == (Some(['t'], !!"est"))
    
[<Test>]
let test_repeat_no_matches () =
    repeat (item_equal 'e') !!"ttttest" == (Some([], !!"ttttest"))

[<Test>]
let test_repeat1 () =
    repeat1 (item_equal 't') !!"ttttest" == (Some(!!"tttt", !!"est"))
    
[<Test>]
let test_repeat1_one_match() =
    repeat1 (item_equal 't') !!"test" == (Some(['t'], !!"est"))

[<Test>]
let test_repeat1_no_matches () =
    repeat1 (item_equal 'e') !!"ttttest" == None
    
[<Test>]
let test_failure_predicate_parser_success() =
    !~ (item_equal 't') !!"test" == None

[<Test>]
let test_failure_predicate_parser_fails () =
    !~ (item_equal 'e') !!"test" == (Some((), !!"test"))
    
[<Test>]
let test_Success_predicate_parser_success() =
    !& (item_equal 't') !!"test" == (Some((), !!"test"))

[<Test>]
let test_success_predicate_parser_fails () =
    !& (item_equal 'e') !!"test" == None
    
[<Test>]
let test_option_predicate_one () =
    !? (item_equal 't') !!"test" == (Some(Some('t'), !!"est"))

[<Test>]
let test_option_predicate_zero () =
    !? (item_equal 'e') !!"test" == (Some(None, !!"test"))

[<Test>]
let test_ignore_left () =
    ((item_equal 't') .>> (item_equal 'e')) !!"test" == (Some('t', !!"st"))

[<Test>]
let test_ignore_left_fails () =
    ((item_equal 'e') .>> (item_equal 'e')) !!"test" == None

[<Test>]
let test_ignore_right () =
    ((item_equal 't') >>. (item_equal 'e')) !!"test" == (Some('e', !!"st"))

[<Test>]
let test_ignore_right_fails () =
    ((item_equal 't') >>. (item_equal 's')) !!"test" == None

[<Test>]
let test_parse_return_value () =
    ((item_equal 't') >>! "hello") !!"test" == (Some("hello", !!"est"))

[<Test>]
let test_parse_return_value_fails () =
    ((item_equal 'q') >>! "hello") !!"test" == None
    
[<Test>]
let test_repeat_until () =
    (repeat_until item (item_equal 's')) !!"test" == (Some(!!"te", !!"t"))
    
[<Test>]
let test_repeat_until_fail_1 () =
    (repeat_until item (item_equal 'q')) !!"test" == None

[<Test>]
let test_repeat_until_fail_2 () =
    (repeat_until (item_equal 'q') (item_equal 's')) !!"test" == None
    
[<Test>]
let test_skip_item () =
    let exp = Some((), !!"est")
    skip_item 't' !!"test" == exp

[<Test>]
let test_skip_item_fail () =
    skip_item 'e' !!"test" == None
    
[<Test>]
let test_skip_items () =
    let exp = Some((), !!"st")
    skip_items !!"te" !!"test" == exp

[<Test>]
let test_skip_items_fail () =
    skip_items !!"ts" !!"test" == None
    
[<Test>]
let test_space () =
    let exp = Some (' ', !!" test")
    space !!"  test" == exp
    
[<Test>]
let test_tab () =
    let exp = Some ('\t', !!" test")
    space !!"\t test" == exp
    
