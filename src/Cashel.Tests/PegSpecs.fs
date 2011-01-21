module PegSpecs

let (!!) str = List.ofSeq str
let chr (c:int) = System.Convert.ToChar(c)

open Cashel
open Cashel.Peg
open NUnit.Framework
open BaseSpecs

[<Test>]
let test_EndOfFile_with_empty_string () = 
    let exp = Some((),[])
    EndOfFile [] == exp
    
[<Test>]
let test_EndOfFile_with_non_empty_string () = 
    EndOfFile !!"test"== None
    
[<Test>]
let test_EndOfLine_with_slashr_slashn () = 
    let exp = Some(!!"\r\n",!!"test")
    EndOfLine !!"\r\ntest" == exp

[<Test>]
let test_EndOfLine_with_slashr () = 
    let exp = Some(!!"\r",!!"test")
    EndOfLine !!"\rtest" == exp

[<Test>]
let test_EndOfLine_with_slashn () = 
    let exp = Some(!!"\n",!!"test")
    EndOfLine !!"\ntest" == exp

[<Test>]
let test_EndOfLine_with_no_slash () = 
    EndOfLine !!"test" == None
    
[<Test>]
let test_EndOfLine_with_slashn_slashr () = 
    let exp = Some(!!"\n",!!"\rtest")
    EndOfLine !!"\n\rtest" == exp

[<Test>]
let test_Space_with_space () = 
    let exp = Some(!!" ",!!"test")
    Space !!" test" == exp

[<Test>]
let test_Space_with_slasht () = 
    let exp = Some(!!"\t",!!"test")
    Space !!"\ttest" == exp

[<Test>]
let test_Space_with_eol () = 
    let exp = Some(!!"\r\n",!!"test")
    Space !!"\r\ntest" == exp

[<Test>]
let test_Comment () = 
    let exp = Some(!!"test comment",!!"more text")
    Comment !!"#test comment\r\nmore text" == exp

[<Test>]
let test_Comment_not_comment () = 
    Comment !!"test comment\r\nmore text" == None

[<Test>]
let test_Spacing_with_no_comment () = 
    Spacing !!"test comment\r\nmore text" == (Some([], !!"test comment\r\nmore text"))

[<Test>]
let test_Spacing_with_comment () = 
    let exp = Some([!!"test comment"],!!"more text")
    Spacing !!"#test comment\r\nmore text" == exp

[<Test>]
let test_Spacing_with_space () = 
    let exp = Some([!!" "],!!"more text")
    Spacing !!" more text" == exp

[<Test>]
let test_Spacing_with_comment_and_space () = 
    let exp = Some([!!" ";!!"test comment"],!!"more text")
    Spacing !!" #test comment\r\nmore text" == exp

[<Test>]
let test_Spacing_with_space_and_comment () = 
    let exp = Some([!!"test comment";[' '];[' '];[' '];[' ']],!!"more text")
    let act = Spacing !!"#test comment\r\n    more text" 
    act == exp

[<Test>]
let test_dot () = DOT !!".test" == (Some('.', !!"test"))

[<Test>]
let test_dot_with_space () = DOT !!". \t  test" == (Some('.', !!"test"))

[<Test>]
let test_dot_with_slasht () = DOT !!".\test" == (Some('.', !!"est"))

[<Test>]
let test_dot_fail () = DOT !!"test" == None

[<Test>]
let test_slash () = SLASH !!"/test" == (Some('/', !!"test"))

[<Test>]
let test_slash_fail () = SLASH !!"test" == None

[<Test>]
let test_LEFTARROW () = 
    let exp = Some(!!"<-",!!"more text")
    LEFTARROW !!"<-more text" == exp

[<Test>]
let test_LEFTARROW_with_space () = 
    let exp = Some(!!"<-",!!"more text")
    LEFTARROW !!"<-\r\nmore text" == exp

[<Test>]
let test_LEFTARROW_fail () = 
    LEFTARROW !!"q<-more text" == None

[<Test>]
let test_Char_with_slashn () =
    let exp = Some('\n', !!"test")
    Char !! @"\ntest" == exp

[<Test>]
let test_Char_with_slashr () =
    let exp = Some('\r', !!"test")
    Char !! @"\rtest" == exp

[<Test>]
let test_Char_with_slasht () =
    let exp = Some('\t', !!"test")
    Char !! @"\ttest" == exp

[<Test>]
let test_Char_with_slash () =
    let exp = Some('\\', !!"test")
    Char !! @"\\test" == exp

[<Test>]
let test_Char_with_no_slash () =
    let exp = Some('t', !!"est")
    let act = Char !! "test" 
    act == exp

[<Test>]
let test_Char_with_slash_onenumber () =
    let exp = Some((chr  0), !!"test")
    Char !! @"\0test" == exp
    
[<Test>]
let test_Char_with_slash_onenumber_too_big () =
    Char !! @"\8test" == None
    
[<Test>]
let test_Char_with_slash_twonumber () =
    let exp = Some(chr (3*8), !!"test")
    Char !! @"\30test" == exp

[<Test>]
let test_Char_with_slash_twonumber_too_big () =
    Char !! @"\81test" == None

[<Test>]
let test_Char_with_slash_twonumber_followed_by_number () =
    let exp = Some(chr (3*8), !!"3test")
    Char !! @"\303test" == exp
    
[<Test>]
let test_Char_with_slash_threenumber () =
    let exp = Some(chr (2*64+3*8), !!"test")
    Char !! @"\230test" == exp

[<Test>]
let test_Char_with_slash_threenumbers_too_big () =
    let act = Char !! @"\833test" 
    act == None

[<Test>]
let test_Range_single () =
    let exp = Some(Single('t'), !!"est")
    Range !! "test" == exp

[<Test>]
let test_Range_dual () =
    let exp = Some(Dual('t', 'e'), !!"st")
    Range !! "t-est" == exp
    
[<Test>]
let test_Range_dual_fail_back_to_single () =
    let exp = Some(Single('t'), !!"-\\st")
    Range !! "t-\\st" == exp

[<Test>]
let test_Range_single_fail () =
    Range !! "\\st" == None
    
[<Test>]
let test_Class_single () =
    Peg.Class !! "[a]test" == (Some([Single('a')], !!"test"))

[<Test>]
let test_Class_single_spacing () =
    Peg.Class !! "[a]\t\ttest" == (Some([Single('a')], !!"test"))
    
[<Test>]
let test_Class_single_range () =
    Peg.Class !! "[a-z]test" == (Some([Dual('a', 'z')], !!"test"))
    
[<Test>]
let test_Class_multiple () =
    Peg.Class !! "[ab-z]test" == (Some([Single('a');Dual('b','z')], !!"test"))

[<Test>]
let test_Class_failure_no_end_bracket () =
    Peg.Class !! "[ab-ztest" == None
    
[<Test>]
let test_Class_failure () =
    Peg.Class !! "ab-z]test" == None

[<Test>]
let test_Literal_single_quote () =
    Peg.Literal !! "'test'  me" == (Some(!!"test", !!"me"))

[<Test>]
let test_Literal_double_quote () =
    Peg.Literal !! "\"test\"  me" == (Some(!!"test", !!"me"))

[<Test>]
let test_Literal_no_end_quote () =
    Peg.Literal !! "\"test  me" == None

[<Test>]
let test_Identifier () =
    Peg.Identifier !! "tE_s9t me" == (Some(!!"tE_s9t", !!"me"))

[<Test>]
let test_Identifier_start_with_underscore () =
    Peg.Identifier !! "_9test me" == (Some(!!"_9test", !!"me"))

[<Test>]
let test_Identifier_fail_start_with_number () =
    Peg.Identifier !! "9test me" == None
    
[<Test>]
let test_Primary_Identifier () =
    pPrimary !!"test me" == (Some(Primary.Identifier(!!"test"), !!"me"))

[<Test>]
let test_Primary_Literal () =
    pPrimary !!"'test' me" == (Some(Primary.Literal(!!"test"), !!"me"))
    
[<Test>]
let test_Primary_expression () =
    let exp = Some(Primary.Class([Dual('t','v');Single('z')]), !!"st me")
    pPrimary !!"[t-vz]  st me" == exp
    
[<Test>]
let test_Primary_class () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=None}
    let exp = Some(Primary.Expression([[si]]), !!"me")
    pPrimary !!"(test) me" == exp

[<Test>]
let test_Primary_dot () =
    pPrimary !!".\test me" == (Some(Primary.Dot, !!"est me"))
   
[<Test>]
let test_SequenceItem () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=None}
    let exp = Some(si, !!"me")
    SequenceItem !!"test me" == exp
 
[<Test>]
let test_SequenceItem_prefix_and_suffix () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=Some(Prefix.And);itemSuffix=Some(Suffix.Question)}
    let exp = Some(si, !!"me")
    SequenceItem !!"&test? me" == exp
    
[<Test>]
let test_SequenceItem_prefix_only () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=Some(Prefix.Not);itemSuffix=None}
    let exp = Some(si, !!"me")
    SequenceItem !!"!test me" == exp    

[<Test>]
let test_SequenceItem_suffix_only () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=Some(Suffix.Star)}
    let exp = Some(si, !!"me")
    SequenceItem !!"test* me" == exp    

[<Test>]
let test_SequenceItem_suffix_only_2 () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=Some(Suffix.Plus)}
    let exp = Some(si, !!"me")
    SequenceItem !!"test+ me" == exp   
     
[<Test>]
let test_Sequence () =
    let createPrim id = {primaryItem=Primary.Identifier(!!id);itemPrefix=None;itemSuffix=None}
    let exp = Some([createPrim "try"; createPrim "test"], !!"me <- now")
    Sequence !!"try test me <- now" == exp
    
[<Test>]  
let test_PEG_grammar () =
    let pegGrammar = !! @"

# Hierarchical syntax
Grammar <- Spacing Definition+ EndOfFile
Definition <- Identifier LEFTARROW Expression
Expression <- Sequence (SLASH Sequence)*
Sequence <- Prefix*
Prefix <- (AND / NOT)? Suffix
Suffix <- Primary (QUESTION / STAR / PLUS)?
Primary <- Identifier !LEFTARROW / OPEN Expression CLOSE / Literal / Class / DOT

# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]

Literal <- ['] (!['] Char)* ['] Spacing / [""] (![""] Char)* [""] Spacing
Class <- '[' (!']' Range)* ']' Spacing
Range <- Char '-' Char / Char
Char <- '\\' [nrt'""\[\]\\] / '\\' [0-2][0-7][0-7] / '\\' [0-7][0-7]? / !'\\' .

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

    let defs, cl = Grammar pegGrammar |> Option.get
    defs |> List.length == 29
    cl == []