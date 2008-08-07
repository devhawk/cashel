#light

let (!!) str = List.of_seq str
let chr = Char.chr

open Xunit
open FsxUnit.Syntax


open DevHawk.Parser.Peg2
    
[<Fact>]
let test_EndOfLine_with_slashr_slashn () = 
    let exp = Some(!!"\r\n",!!"test")
    EndOfLine !!"\r\ntest" |> should equal exp

[<Fact>]
let test_EndOfLine_with_slashr () = 
    let exp = Some(!!"\r",!!"test")
    EndOfLine !!"\rtest" |> should equal exp

[<Fact>]
let test_EndOfLine_with_slashn () = 
    let exp = Some(!!"\n",!!"test")
    EndOfLine !!"\ntest" |> should equal exp

[<Fact>]
let test_EndOfLine_with_no_slash () = 
    EndOfLine !!"test" |> should equal None
    
[<Fact>]
let test_EndOfLine_with_slashn_slashr () = 
    let exp = Some(!!"\n",!!"\rtest")
    EndOfLine !!"\n\rtest" |> should equal exp

[<Fact>]
let test_Space_with_space () = 
    let exp = Some(!!" ",!!"test")
    Space !!" test" |> should equal exp

[<Fact>]
let test_Space_with_slasht () = 
    let exp = Some(!!"\t",!!"test")
    Space !!"\ttest" |> should equal exp

[<Fact>]
let test_Space_with_eol () = 
    let exp = Some(!!"\r\n",!!"test")
    Space !!"\r\ntest" |> should equal exp

[<Fact>]
let test_Comment () = 
    let exp = Some(!!"test comment",!!"more text")
    Comment !!"#test comment\r\nmore text" |> should equal exp

[<Fact>]
let test_Comment_not_comment () = 
    Comment !!"test comment\r\nmore text" |> should equal None

[<Fact>]
let test_Spacing_with_no_comment () = 
    Spacing !!"test comment\r\nmore text" |> should equal (Some([], !!"test comment\r\nmore text"))

[<Fact>]
let test_Spacing_with_comment () = 
    let exp = Some([!!"test comment"],!!"more text")
    Spacing !!"#test comment\r\nmore text" |> should equal exp

[<Fact>]
let test_Spacing_with_space () = 
    let exp = Some([!!" "],!!"more text")
    Spacing !!" more text" |> should equal exp

[<Fact>]
let test_Spacing_with_comment_and_space () = 
    let exp = Some([!!" ";!!"test comment"],!!"more text")
    Spacing !!" #test comment\r\nmore text" |> should equal exp

[<Fact>]
let test_Spacing_with_space_and_comment () = 
    let exp = Some([!!"test comment";[' '];[' '];[' '];[' ']],!!"more text")
    let act = Spacing !!"#test comment\r\n    more text" 
    act |> should equal exp

[<Fact>]
let test_dot () = DOT !!".test" |> should equal (Some('.', !!"test"))

[<Fact>]
let test_dot_with_space () = DOT !!". \t  test" |> should equal (Some('.', !!"test"))

[<Fact>]
let test_dot_with_slasht () = DOT !!".\test" |> should equal (Some('.', !!"est"))

[<Fact>]
let test_dot_fail () = DOT !!"test" |> should equal None

[<Fact>]
let test_slash () = SLASH !!"/test" |> should equal (Some('/', !!"test"))

[<Fact>]
let test_slash_fail () = SLASH !!"test" |> should equal None

[<Fact>]
let test_LEFTARROW () = 
    let exp = Some(!!"<-",!!"more text")
    LEFTARROW !!"<-more text" |> should equal exp

[<Fact>]
let test_LEFTARROW_with_space () = 
    let exp = Some(!!"<-",!!"more text")
    LEFTARROW !!"<-\r\nmore text" |> should equal exp

[<Fact>]
let test_LEFTARROW_fail () = 
    LEFTARROW !!"q<-more text" |> should equal None

[<Fact>]
let test_Char_with_slashn () =
    let exp = Some('\n', !!"test")
    _Char !! @"\ntest" |> should equal exp

[<Fact>]
let test_Char_with_slashr () =
    let exp = Some('\r', !!"test")
    _Char !! @"\rtest" |> should equal exp

[<Fact>]
let test_Char_with_slasht () =
    let exp = Some('\t', !!"test")
    _Char !! @"\ttest" |> should equal exp

    
[<Fact>]
let test_Char_with_slash () =
    let exp = Some('\\', !!"test")
    _Char !! @"\\test" |> should equal exp

[<Fact>]
let test_Char_with_no_slash () =
    let exp = Some('t', !!"est")
    let act = _Char !! "test" 
    act |> should equal exp

[<Fact>]
let test_Char_with_unicode_specification_0000 () =
    let exp = Some((chr  0x0000), !!"test")
    _Char !! @"\u0000test" |> should equal exp
    
[<Fact>]
let test_Char_with_unicode_specification_abcd () =
    let exp = Some((chr  0xabcd), !!"test")
    _Char !! @"\uabcdtest" |> should equal exp
    
[<Fact>]
let test_Char_with_unicode_specification_ABCD () =
    let exp = Some((chr  0xABCD), !!"test")
    _Char !! @"\uABCDtest" |> should equal exp

[<Fact>]
let test_Range_single () =
    let exp = Some(Single('t'), !!"est")
    _Range !! "test" |> should equal exp

[<Fact>]
let test_Range_dual () =
    let exp = Some(Dual('t', 'e'), !!"st")
    _Range !! "t-est" |> should equal exp
    
[<Fact>]
let test_Range_dual_fail_back_to_single () =
    let exp = Some(Single('t'), !!"-\\st")
    _Range !! "t-\\st" |> should equal exp

[<Fact>]
let test_Range_single_fail () =
    _Range !! "\\st" |> should equal None
    
[<Fact>]
let test_Class_single () =
    _Class !! "[a]test" |> should equal (Some([Single('a')], !!"test"))

[<Fact>]
let test_Class_single_spacing () =
    _Class !! "[a]\t\ttest" |> should equal (Some([Single('a')], !!"test"))
    
[<Fact>]
let test_Class_single_range () =
    _Class !! "[a-z]test" |> should equal (Some([Dual('a', 'z')], !!"test"))
    
[<Fact>]
let test_Class_multiple () =
    _Class !! "[ab-z]test" |> should equal (Some([Single('a');Dual('b','z')], !!"test"))

[<Fact>]
let test_Class_failure_no_end_bracket () =
    _Class !! "[ab-ztest" |> should equal None
    
[<Fact>]
let test_Class_failure () =
    _Class !! "ab-z]test" |> should equal None

[<Fact>]
let test_Literal_single_quote () =
    _Literal !! "'test'  me" |> should equal (Some("test", !!"me"))

[<Fact>]
let test_Literal_double_quote () =
    _Literal !! "\"test\"  me" |> should equal (Some("test", !!"me"))

[<Fact>]
let test_Literal_no_end_quote () =
    _Literal !! "\"test  me" |> should equal None

[<Fact>]
let test_Identifier () =
    _Identifier !! "tE_s9t me" |> should equal (Some("tE_s9t", !!"me"))

[<Fact>]
let test_Identifier_start_with_underscore () =
    _Identifier !! "_9test me" |> should equal (Some("_9test", !!"me"))

[<Fact>]
let test_Identifier_fail_start_with_number () =
    _Identifier !! "9test me" |> should equal None
    
[<Fact>]
let test_Primary_Identifier () =
    _Primary !!"test me" |> should equal (Some(Identifier("test"), !!"me"))

[<Fact>]
let test_Primary_Literal () =
    _Primary !!"'test' me" |> should equal (Some(Literal("test"), !!"me"))
    
[<Fact>]
let test_Primary_class () =
    let exp = Some(Class([Dual('t','v');Single('z')]), !!"st me")
    _Primary !!"[t-vz]  st me" |> should equal exp
    
//TODO - test primary produciton
[<Fact>]
let test_Primary_dot () =
    _Primary !!".\test me" |> should equal (Some(Primary.Dot, !!"est me"))
    

[<Fact>]
let test_PatternItem () =
    let exp = Some({item=Identifier("test");prefix=None;arity=None}, !!"me")
    _PatternItem !!"test me" |> should equal exp
    
[<Fact>]
let test_PatternItem_plus_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(OneOrMore)}, !!"me")
    _PatternItem !!"test+ me" |> should equal exp

[<Fact>]
let test_PatternItem_star_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(ZeroOrMore)}, !!"me")
    _PatternItem !!"test* me" |> should equal exp
    
[<Fact>]
let test_PatternItem_question_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(ZeroOrOne)}, !!"me")
    _PatternItem !!"test? me" |> should equal exp      
    
[<Fact>]
let test_PatternItem_and_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(SuccessPredicate);arity=None}, !!"me")
    _PatternItem !!"&test me" |> should equal exp

[<Fact>]
let test_PatternItem_bang_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(FailurePredicate);arity=None}, !!"me")
    _PatternItem !!"!test me" |> should equal exp
    
[<Fact>]
let test_PatternItem_variable_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(Variable("qa"));arity=None}, !!"me")
    _PatternItem !!"qa:test me" |> should equal exp   
    
[<Fact>]
let test_PatternItem_variable_prefix_star_suffix () =
    let exp = Some({item=Identifier("test");prefix=Some(Variable("qa"));arity=Some(ZeroOrMore)}, !!"me")
    _PatternItem !!"qa:test* me" |> should equal exp   

[<Fact>]
let test_Production_one_pattern_item_no_action () =
    let exp = Some({pattern=[{item=Identifier("test");prefix=None;arity=None}]; action=None}, !!"/")
    _Production !!"test /" |> should equal exp
    
[<Fact>]
let test_Production_three_pattern_items_no_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let exp = Some({pattern=pl; action=None}, !!"/")
    _Production !!"test me now/" |> should equal exp
    
[<Fact>]
let test_Production_one_pattern_item_with_action () =
    let exp = Some({pattern=[{item=Identifier("test");prefix=None;arity=None}]; action=Some("action")}, !!"/")
    _Production !!"test => action/" |> should equal exp
    
[<Fact>]
let test_Production_three_pattern_items_with_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let exp = Some({pattern=pl; action=Some("action")}, !!"/")
    _Production !!"test me now => action/" |> should equal exp
    
[<Fact>]
let test_Rule_one_production () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = {pattern=pl; action=None}
    let exprule = {name="rulename";productions=[prod]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now ; foobar" 
    act |> should equal exp
    
[<Fact>]
let test_Rule_two_productions () =
    let prod1 = {pattern=[{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]; action=None}
    let prod2 = {pattern=[{item=Identifier("another");prefix=None;arity=None};{item=Identifier("test");prefix=None;arity=None}]; action=None}
    let exprule = {name="rulename";productions=[prod1;prod2]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now / another test; foobar" 
    act |> should equal exp
    
[<Fact>]
let test_Rule_one_production_with_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = {pattern=pl; action=Some("Action")}
    let exprule = {name="rulename";productions=[prod]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now => Action; foobar" 
    act |> should equal exp
    
[<Fact>]
let test_Rule_two_productions_with_actions () =
    let prod1 = {pattern=[{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]; action=Some("Action1")}
    let prod2 = {pattern=[{item=Identifier("another");prefix=None;arity=None};{item=Identifier("test");prefix=None;arity=None}]; action=Some("Action2")}
    let exprule = {name="rulename";productions=[prod1;prod2]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now => Action1/ another test => Action2; foobar" 
    act |> should equal exp
    
    
(*[<Fact>]  
let test_PEG_grammar () =
    let peg_grammar = !! @"

# Hierarchical syntax
Grammar <- Spacing Definition+ EndOfFile
Definition <- Identifier LEFTARROW Expression
Expression <- Sequence (SLASH Sequence)*
Sequence <- Prefix*
Prefix <- (AND / NOT)? arity
arity <- Primary (QUESTION / STAR / PLUS)?
Primary <- Identifier !LEFTARROW / OPEN Expression CLOSE / Literal / Class / DOT

# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]

Literal <- ['] (!['] _Char)* ['] Spacing / [""] (![""] _Char)* [""] Spacing
Class <- '[' (!']' _Range)* ']' Spacing
_Range <- _Char '-' _Char / _Char
_Char <- '\\' [nrt'""\[\]\\] / '\\' [0-2][0-7][0-7] / '\\' [0-7][0-7]? / !'\\' .

LEFTARROW <- '<-' Spacing
SLASH <- '/' Spacing
AND <- '&' Spacing
NOT <- '!' Spacing
QUESTION <- '?' Spacing
STAR <- '*' Spacing
PLUS <- '+' Spacing
OPEN <- '(' Spacing
CLOSE <- ')' Spacing
DOT <- '.' Spacing

Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* EndOfLine
Space <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !." 

    let Some(defs, cl) = peg_grammar |> Grammar
    defs |> List.length |> should equal 29
    cl |> should equal []*)


