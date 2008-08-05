#light

let (!!) str = List.of_seq str

open Xunit
open FsxUnit.Syntax

open DevHawk.Parser.Primitives

[<Fact>]
let test_item () =
    let exp = Some('t', !!"est")
    item !!"test" |> should equal exp
    
[<Fact>]
let test_item_empty_list () =
    item [] |> should equal None

[<Fact>]
let test_item_single_item () =
    let exp = Some('t', [])
    item ['t'] |> should equal exp

[<Fact>]
let test_ignore_with_item () =
    let exp = Some((), !!"est")
    ignore item !!"test" |> should equal exp 

[<Fact>]
let test_listify_with_item () =
    let exp = Some(['t'], !!"est")
    listify item !!"test" |> should equal exp   
    
[<Fact>]
let test_satisy_simple_predicate () =
    let exp = Some('t', !!"est")
    satisfy item (fun x -> x = 't') !!"test" |> should equal exp

[<Fact>]
let test_satisy_failure_predicate () =
    satisfy item (fun x -> x = 'e') !!"test" |> should equal None
    
[<Fact>]
let test_any_of_success_predicate () =
    any_of ['q'..'v'] !!"test" |> should equal (Some('t', !!"est"))
    
[<Fact>]
let test_any_of_failure_predicate () =
    any_of ['a'..'e'] !!"test" |> should equal None
    
[<Fact>]
let test_item_equal () =
    item_equal 't' !!"test" |> should equal (Some('t', !!"est"))

[<Fact>]
let test_item_equal_failure () =
    item_equal 'e' !!"test" |> should equal None
    
[<Fact>]
let test_items_equal () =
    items_equal !!"test" !!"test me" |> should equal (Some(!!"test", !!" me"))

[<Fact>]
let test_items_equal_failue () =
    items_equal !!"test" !!"tesp me" |> should equal None
    
[<Fact>]
let test_eof () = 
    eof [] |> should equal (Some((),[]))
    
[<Fact>]
let test_eof_fails_not_at_end () = 
    eof !!"test" |> should equal None
    
[<Fact>]
let test_repeat () =
    repeat (item_equal 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))

[<Fact>]
let test_repeat_one_match() =
    repeat (item_equal 't') !!"test" |> should equal (Some(['t'], !!"est"))
    
[<Fact>]
let test_repeat_no_matches () =
    repeat (item_equal 'e') !!"ttttest" |> should equal (Some([], !!"ttttest"))

[<Fact>]
let test_repeat1 () =
    repeat1 (item_equal 't') !!"ttttest" |> should equal (Some(!!"tttt", !!"est"))
    
[<Fact>]
let test_repeat1_one_match() =
    repeat1 (item_equal 't') !!"test" |> should equal (Some(['t'], !!"est"))

[<Fact>]
let test_repeat1_no_matches () =
    repeat1 (item_equal 'e') !!"ttttest" |> should equal None
    
[<Fact>]
let test_failure_predicate_parser_success() =
    !~ (item_equal 't') !!"test" |> should equal None

[<Fact>]
let test_failure_predicate_parser_fails () =
    !~ (item_equal 'e') !!"test" |> should equal (Some((), !!"test"))
    
[<Fact>]
let test_Success_predicate_parser_success() =
    !& (item_equal 't') !!"test" |> should equal (Some((), !!"test"))

[<Fact>]
let test_success_predicate_parser_fails () =
    !& (item_equal 'e') !!"test" |> should equal None
    
[<Fact>]
let test_option_predicate_one () =
    !? (item_equal 't') !!"test" |> should equal (Some(Some('t'), !!"est"))

[<Fact>]
let test_option_predicate_zero () =
    !? (item_equal 'e') !!"test" |> should equal (Some(None, !!"test"))

[<Fact>]
let test_ignore_left () =
    ((item_equal 't') .>> (item_equal 'e')) !!"test" |> should equal (Some('t', !!"st"))

[<Fact>]
let test_ignore_left_fails () =
    ((item_equal 'e') .>> (item_equal 'e')) !!"test" |> should equal None

[<Fact>]
let test_ignore_right () =
    ((item_equal 't') >>. (item_equal 'e')) !!"test" |> should equal (Some('e', !!"st"))

[<Fact>]
let test_ignore_right_fails () =
    ((item_equal 't') >>. (item_equal 's')) !!"test" |> should equal None
