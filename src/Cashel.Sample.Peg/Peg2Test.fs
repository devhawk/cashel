module Peg2Test

let (!!) str = List.ofSeq str
let chr (c:int) = System.Convert.ToChar(c)

let peg2grammar = @"
PEG2 
{
    Grammar <- Spacing i:Identifier OCURLY r:Rule+ CCURLY EndOfFile => ir;
    Rule <- i:Identifier LEFTARROW p1:Production p2:(SLASH p3:Production => p3)* SEMICOLON => ip1p2;
    Production <- pi:PatternItem+ a:(RIGHTARROW a:Action => a)? => pia;
    Suffix <- QUESTION => ZeroOrOne / STAR => ZeroOrMore / PLUS => OneOrMore;
    Prefix <- AND => SucessPred / NOT => FailPred / i:Identifier COLON => il;
    PatternItem <- pre:Prefix? pri:Primary suf:Suffix? => pre_pri_suf;
    Primary <- i:Identifier => i / OPEN p:Production CLOSE => p/ l:Literal => l/ c:Class => c / DOT => dot;
    Action <- i:Identifier => i;
    Identifier <- p1:[a-zA-Z_] pr:[_a-zA-Z0-9]* Spacing => p1pr;
    Literal    <- ['] cl:(!['] c:Char=>c)* ['] Spacing => cl / [""] cl:(![""] c:Char=>c)* [""] Spacing => cl ;
    Class      <- '[' r:(!']' r:Range=>r)* ']' Spacing => r;
    Range      <- c1:Char '-' c2:Char => c1c2 / c:Char => c;
    Char <- '\\' c:[nrt'""\[\]\\] => c / '\\u' h1:HexDigit h2:HexDigit h3:HexDigit h4:HexDigit =>h1h2h3h4/ !'\\' c:. => c;
    HexDigit <- c:[a-fA-F0-9] => c;
    LEFTARROW  <- '<-' Spacing;
    CLOSE      <- ')' Spacing;
    OPEN       <- '(' Spacing;
    SLASH      <- '/' Spacing;
    RIGHTARROW  <- '=>' Spacing;
    COLON       <- ':' Spacing;
    SEMICOLON       <- ';' Spacing;
    DOT        <- '.' Spacing;
    OPEN       <- '(' Spacing;
    CLOSE      <- ')' Spacing;
    AND        <- '&' Spacing;
    NOT       <- '!' Spacing;
    QUESTION   <- '?' Spacing;
    STAR       <- '*' Spacing;
    PLUS       <- '+' Spacing;
    Spacing    <- SpaceOrComment*;
    SpaceOrComment    <- Space / Comment;
    Comment    <- '#' (!EndOfLine .)* EndOfLine;
    Space      <- ' ' / '\t' / EndOfLine;
    EndOfLine  <- '\r\n' / '\n' / '\r';
    EndOfFile  <- !.;
}

"

open NUnit.Framework
open FsUnit

open Cashel
open Cashel.Sample.Peg2
    
let (>|>) act exp = 
    match exp with
    | None ->  act |> should equal exp
    | Some(expV, expT) -> 
        match act with
        | Some(actV, actT) ->
            actV |> should equal expV
            let expT' = new System.String(List.toArray expT)
            let actT' = new System.String(List.toArray actT)
            actT' |> should equal expT'
        | _ -> act |> should equal exp

    
[<Test>]
let test_eof () = 
    _EndOfFile [] = (Some((),[]))|> should be True
    
[<Test>]
let test_eof_fails_not_at_end () = 
    _EndOfFile !!"test" |> should equal None
    
[<Test>]
let test_EndOfLine_with_slashr_slashn () = 
    let exp = Some((),!!"test")
    _EndOfLine !!"\r\ntest" >|> exp

[<Test>]
let test_EndOfLine_with_slashr () = 
    let exp = Some((),!!"test")
    _EndOfLine !!"\rtest" >|> exp

[<Test>]
let test_EndOfLine_with_slashn () = 
    let exp = Some((),!!"test")
    _EndOfLine !!"\ntest" >|> exp

[<Test>]
let test_EndOfLine_with_no_slash () = 
    _EndOfLine !!"test" >|> None
    
[<Test>]
let test_EndOfLine_with_slashn_slashr () = 
    let exp = Some((),!!"\rtest")
    _EndOfLine !!"\n\rtest" >|> exp

[<Test>]
let test_Space_with_space () = 
    let exp = Some((),!!"test")
    _Space !!" test" >|> exp

[<Test>]
let test_Space_with_slasht () = 
    let exp = Some((),!!"test")
    _Space !!"\ttest" >|> exp

[<Test>]
let test_Space_with_eol () = 
    let exp = Some((),!!"test")
    _Space !!"\r\ntest" >|> exp

[<Test>]
let test_Comment () = 
    let exp = Some((),!!"more text")
    _Comment !!"#test _Comment\r\nmore text" >|> exp

[<Test>]
let test_Comment_not_comment () = 
    _Comment !!"test _Comment\r\nmore text" >|> None

[<Test>]
let test_Spacing_with_no_comment () = 
    _Spacing !!"test _Comment\r\nmore text" >|> (Some((), !!"test _Comment\r\nmore text"))

[<Test>]
let test_Spacing_with_comment () = 
    let exp = Some((),!!"more text")
    _Spacing !!"#test _Comment\r\nmore text" >|> exp

[<Test>]
let test_Spacing_with_space () = 
    let exp = Some((),!!"more text")
    _Spacing !!" more text" >|> exp

[<Test>]
let test_Spacing_with_comment_and_space () = 
    let exp = Some((),!!"more text")
    _Spacing !!" #test _Comment\r\nmore text" >|> exp

[<Test>]
let test_Spacing_with_space_and_comment () = 
    let exp = Some((),!!"more text")
    let act = _Spacing !!"#test _Comment\r\n    more text" 
    act >|> exp

[<Test>]
let test_dot () = _DOT !!".test" >|> (Some((), !!"test"))

[<Test>]
let test_dot_with_space () = _DOT !!". \t  test" >|> (Some((), !!"test"))

[<Test>]
let test_dot_with_slasht () = _DOT !!".\test" >|> (Some((), !!"est"))

[<Test>]
let test_dot_fail () = _DOT !!"test" >|> None

[<Test>]
let test_slash () = _SLASH !!"/test" >|> (Some((), !!"test"))

[<Test>]
let test_slash_fail () = _SLASH !!"test" >|> None

[<Test>]
let test_LEFTARROW () = 
    let exp = Some((),!!"more text")
    _LEFTARROW !!"<-more text" >|> exp

[<Test>]
let test_LEFTARROW_with_space () = 
    let exp = Some((),!!"more text")
    _LEFTARROW !!"<-\r\nmore text" >|> exp

[<Test>]
let test_LEFTARROW_fail () = 
    _LEFTARROW !!"q<-more text" >|> None

[<Test>]
let test_Char_with_slashn () =
    let exp = Some('\n', !!"test")
    _Char !! @"\ntest" >|> exp

[<Test>]
let test_Char_with_slashr () =
    let exp = Some('\r', !!"test")
    _Char !! @"\rtest" >|> exp

[<Test>]
let test_Char_with_slasht () =
    let exp = Some('\t', !!"test")
    _Char !! @"\ttest" >|> exp

    
[<Test>]
let test_Char_with_slash () =
    let exp = Some('\\', !!"test")
    _Char !! @"\\test" >|> exp

[<Test>]
let test_Char_with_no_slash () =
    let exp = Some('t', !!"est")
    _Char !! "test" >|> exp

[<Test>]
let test_Char_with_unicode_specification_0000 () =
    let exp = Some((chr  0x0000), !!"test")
    _Char !! @"\u0000test" >|> exp
    
[<Test>]
let test_Char_with_unicode_specification_abcd () =
    let exp = Some((chr  0xabcd), !!"test")
    _Char !! @"\uabcdtest" >|> exp
    
[<Test>]
let test_Char_with_unicode_specification_ABCD () =
    let exp = Some((chr  0xABCD), !!"test")
    _Char !! @"\uABCDtest" >|> exp

[<Test>]
let test_Range_single () =
    let exp = Some(Single('t'), !!"est")
    _Range !! "test" >|> exp

[<Test>]
let test_Range_dual () =
    let exp = Some(Dual('t', 'e'), !!"st")
    _Range !! "t-est" >|> exp
    
[<Test>]
let test_Range_dual_fail_back_to_single () =
    let exp = Some(Single('t'), !!"-\\st")
    _Range !! "t-\\st" >|> exp

[<Test>]
let test_Range_single_fail () =
    _Range !! "\\st" >|> None
    
[<Test>]
let test_Class_single () =
    _Class !! "[a]test" >|> (Some([Single('a')], !!"test"))

[<Test>]
let test_Class_single_spacing () =
    _Class !! "[a]\t\ttest" >|> (Some([Single('a')], !!"test"))
    
[<Test>]
let test_Class_single_range () =
    _Class !! "[a-z]test" >|> (Some([Dual('a', 'z')], !!"test"))
    
[<Test>]
let test_Class_multiple () =
    _Class !! "[ab-z]test" >|> (Some([Single('a');Dual('b','z')], !!"test"))

[<Test>]
let test_Class_failure_no_end_bracket () =
    _Class !! "[ab-ztest" >|> None
    
[<Test>]
let test_Class_failure () =
    _Class !! "ab-z]test" >|> None

[<Test>]
let test_Literal_single_quote () =
    _Literal !! "'test'  me" >|> (Some("test", !!"me"))

[<Test>]
let test_Literal_double_quote () =
    _Literal !! "\"test\"  me" >|> (Some("test", !!"me"))

[<Test>]
let test_Literal_no_end_quote () =
    _Literal !! "\"test  me" >|> None

[<Test>]
let test_Identifier () =
    _Identifier !! "tE_s9t me" >|> (Some("tE_s9t", !!"me"))

[<Test>]
let test_Identifier_start_with_underscore () =
    _Identifier !! "_9test me" >|> (Some("_9test", !!"me"))

[<Test>]
let test_Identifier_fail_start_with_number () =
    _Identifier !! "9test me" >|> None
    
[<Test>]
let test_Primary_Identifier () =
    _Primary !!"test me" >|> (Some(Identifier("test"), !!"me"))

[<Test>]
let test_Primary_Literal () =
    _Primary !!"'test' me" >|> (Some(Literal("test"), !!"me"))
    
[<Test>]
let test_Primary_class () =
    let exp = Some(Class([Dual('t','v');Single('z')]), !!"st me")
    _Primary !!"[t-vz]  st me" >|> exp

[<Test>]
let test_Primary_production () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = Production({pattern=pl; action=Some("action3")})
    let exp = Some(prod, !!"more testing")
    _Primary !!"(test me now => action3)? more testing" >|> Some(prod, !!"? more testing")
    

[<Test>]
let test_Primary_dot () =
    _Primary !!".\test me" >|> (Some(Dot, !!"est me"))
    
[<Test>]
let test_Primary_dot_extra_debug () =
    let input = !!".\test me" 
    let exp = (Some(Dot, !!"est me"))
    let act = _Primary input 
    try
        act >|> exp
    with ex ->
        let ev, et = exp |> Option.get
        let av, at = act |> Option.get
        let msg = sprintf "\ninput:\n%O \nExpected value:\n%O (%s) \nActual value:\n%O (%s)" (new System.String(List.toArray input)) ev (new System.String(List.toArray et)) av (new System.String(List.toArray at))
        Assert.False(true,  msg)


[<Test>]
let test_PatternItem () =
    let exp = Some({item=Identifier("test");prefix=None;arity=None}, !!"me")
    _PatternItem !!"test me" >|> exp
    
[<Test>]
let test_patternitem_production () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = Production({pattern=pl; action=Some("action3")})
    let pi = {item=prod;prefix=Some(Variable("s"));arity=Some(ZeroOrOne)}
    _PatternItem !!"s:(test me now => action3)? more testing" >|> Some(pi, !!"more testing")

    
[<Test>]
let test_PatternItem_plus_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(OneOrMore)}, !!"me")
    _PatternItem !!"test+ me" >|> exp

[<Test>]
let test_PatternItem_star_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(ZeroOrMore)}, !!"me")
    _PatternItem !!"test* me" >|> exp
    
[<Test>]
let test_PatternItem_question_suffix () =
    let exp = Some({item=Identifier("test");prefix=None;arity=Some(ZeroOrOne)}, !!"me")
    _PatternItem !!"test? me" >|> exp      
    
[<Test>]
let test_PatternItem_and_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(SuccessPredicate);arity=None}, !!"me")
    _PatternItem !!"&test me" >|> exp

[<Test>]
let test_PatternItem_bang_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(FailurePredicate);arity=None}, !!"me")
    _PatternItem !!"!test me" >|> exp
    
[<Test>]
let test_PatternItem_variable_prefix () =
    let exp = Some({item=Identifier("test");prefix=Some(Variable("qa"));arity=None}, !!"me")
    _PatternItem !!"qa:test me" >|> exp   
    
[<Test>]
let test_PatternItem_variable_prefix_star_suffix () =
    let exp = Some({item=Identifier("test");prefix=Some(Variable("qa"));arity=Some(ZeroOrMore)}, !!"me")
    _PatternItem !!"qa:test* me" >|> exp   

[<Test>]
let test_Production_one_pattern_item_no_action () =
    let exp = Some({pattern=[{item=Identifier("test");prefix=None;arity=None}]; action=None}, !!"/")
    _Production !!"test /" >|> exp
    
[<Test>]
let test_Production_three_pattern_items_no_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let exp = Some({pattern=pl; action=None}, !!"/")
    _Production !!"test me now/" >|> exp
    
[<Test>]
let test_Production_one_pattern_item_with_action () =
    let exp = Some({pattern=[{item=Identifier("test");prefix=None;arity=None}]; action=Some("action")}, !!"/")
    _Production !!"test => action/" >|> exp
    
[<Test>]
let test_Production_three_pattern_items_with_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let exp = Some({pattern=pl; action=Some("action")}, !!"/")
    _Production !!"test me now => action/" >|> exp
    
[<Test>]
let test_Rule_one_production () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = {pattern=pl; action=None}
    let exprule = {name="rulename";productions=[prod]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now ; foobar" 
    act >|> exp
    
[<Test>]
let test_Rule_two_productions () =
    let prod1 = {pattern=[{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]; action=None}
    let prod2 = {pattern=[{item=Identifier("another");prefix=None;arity=None};{item=Identifier("test");prefix=None;arity=None}]; action=None}
    let exprule = {name="rulename";productions=[prod1;prod2]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now / another test; foobar" 
    act >|> exp
    
[<Test>]
let test_Rule_one_production_with_action () =
    let pl = [{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]
    let prod = {pattern=pl; action=Some("Action")}
    let exprule = {name="rulename";productions=[prod]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now => Action; foobar" 
    act >|> exp
    
[<Test>]
let test_Rule_two_productions_with_actions () =
    let prod1 = {pattern=[{item=Identifier("test");prefix=None;arity=None};{item=Identifier("me");prefix=None;arity=None};{item=Identifier("now");prefix=None;arity=None}]; action=Some("Action1")}
    let prod2 = {pattern=[{item=Identifier("another");prefix=None;arity=None};{item=Identifier("test");prefix=None;arity=None}]; action=Some("Action2")}
    let exprule = {name="rulename";productions=[prod1;prod2]}
    let exp = Some(exprule, !!"foobar")
    let act = _Rule !!"rulename <- test me now => Action1/ another test => Action2; foobar" 
    act >|> exp
    
    
[<Test>]
let test_sample_grammar () =
    let g, cl = _Grammar !!peg2grammar |> Option.get
    let {name=id; rules=rl} = g
    Assert.AreEqual("PEG2", id)
    Assert.AreEqual(35, (List.length rl))
    Assert.AreEqual([], cl)

[<Test>]
let test_parse () =
    let g = Parse peg2grammar |> Option.get
    let {name=id; rules=rl} = g
    Assert.AreEqual("PEG2", id)
    Assert.AreEqual(35, (List.length rl))
