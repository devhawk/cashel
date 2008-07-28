#light

let (!!) str = List.of_seq str
let chr = Char.chr

open Xunit
open FsxUnit.Syntax

open DevHawk.Parser
open DevHawk.Parser.Peg



[<Fact>]
let test_EndOfFile_with_empty_string () = 
    let exp = Some((),[])
    EndOfFile [] |> should equal exp
    
[<Fact>]
let test_EndOfFile_with_non_empty_string () = 
    EndOfFile !!"test"|> should equal None
    
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
    Char !! @"\ntest" |> should equal exp

[<Fact>]
let test_Char_with_slashr () =
    let exp = Some('\r', !!"test")
    Char !! @"\rtest" |> should equal exp

[<Fact>]
let test_Char_with_slasht () =
    let exp = Some('\t', !!"test")
    Char !! @"\ttest" |> should equal exp

    
[<Fact>]
let test_Char_with_slash () =
    let exp = Some('\\', !!"test")
    Char !! @"\\test" |> should equal exp

[<Fact>]
let test_Char_with_no_slash () =
    let exp = Some('t', !!"est")
    let act = Char !! "test" 
    act |> should equal exp

[<Fact>]
let test_Char_with_slash_onenumber () =
    let exp = Some((chr  0), !!"test")
    Char !! @"\0test" |> should equal exp
    
[<Fact>]
let test_Char_with_slash_onenumber_too_big () =
    Char !! @"\8test" |> should equal None
    
[<Fact>]
let test_Char_with_slash_twonumber () =
    let exp = Some(chr (3*8), !!"test")
    Char !! @"\30test" |> should equal exp

[<Fact>]
let test_Char_with_slash_twonumber_too_big () =
    Char !! @"\81test" |> should equal None

[<Fact>]
let test_Char_with_slash_twonumber_followed_by_number () =
    let exp = Some(chr (3*8), !!"3test")
    Char !! @"\303test" |> should equal exp
    
[<Fact>]
let test_Char_with_slash_threenumber () =
    let exp = Some(chr (2*64+3*8), !!"test")
    Char !! @"\230test" |> should equal exp

[<Fact>]
let test_Char_with_slash_threenumbers_too_big () =
    let act = Char !! @"\833test" 
    act |> should equal None

[<Fact>]
let test_Range_single () =
    let exp = Some(Single('t'), !!"est")
    Range !! "test" |> should equal exp

[<Fact>]
let test_Range_dual () =
    let exp = Some(Dual('t', 'e'), !!"st")
    Range !! "t-est" |> should equal exp
    
[<Fact>]
let test_Range_dual_fail_back_to_single () =
    let exp = Some(Single('t'), !!"-\\st")
    Range !! "t-\\st" |> should equal exp

[<Fact>]
let test_Range_single_fail () =
    Range !! "\\st" |> should equal None
    
[<Fact>]
let test_Class_single () =
    Peg.Class !! "[a]test" |> should equal (Some([Single('a')], !!"test"))

[<Fact>]
let test_Class_single_spacing () =
    Peg.Class !! "[a]\t\ttest" |> should equal (Some([Single('a')], !!"test"))
    
[<Fact>]
let test_Class_single_range () =
    Peg.Class !! "[a-z]test" |> should equal (Some([Dual('a', 'z')], !!"test"))
    
[<Fact>]
let test_Class_multiple () =
    Peg.Class !! "[ab-z]test" |> should equal (Some([Single('a');Dual('b','z')], !!"test"))

[<Fact>]
let test_Class_failure_no_end_bracket () =
    Peg.Class !! "[ab-ztest" |> should equal None
    
[<Fact>]
let test_Class_failure () =
    Peg.Class !! "ab-z]test" |> should equal None

[<Fact>]
let test_Literal_single_quote () =
    Peg.Literal !! "'test'  me" |> should equal (Some(!!"test", !!"me"))

[<Fact>]
let test_Literal_double_quote () =
    Peg.Literal !! "\"test\"  me" |> should equal (Some(!!"test", !!"me"))

[<Fact>]
let test_Literal_no_end_quote () =
    Peg.Literal !! "\"test  me" |> should equal None

[<Fact>]
let test_Identifier () =
    Peg.Identifier !! "tE_s9t me" |> should equal (Some(!!"tE_s9t", !!"me"))

[<Fact>]
let test_Identifier_start_with_underscore () =
    Peg.Identifier !! "_9test me" |> should equal (Some(!!"_9test", !!"me"))

[<Fact>]
let test_Identifier_fail_start_with_number () =
    Peg.Identifier !! "9test me" |> should equal None
    
[<Fact>]
let test_Primary_Identifier () =
    pPrimary !!"test me" |> should equal (Some(Identifier(!!"test"), !!"me"))

[<Fact>]
let test_Primary_Literal () =
    pPrimary !!"'test' me" |> should equal (Some(Literal(!!"test"), !!"me"))
    
[<Fact>]
let test_Primary_expression () =
    let exp = Some(Class([Dual('t','v');Single('z')]), !!"st me")
    pPrimary !!"[t-vz]  st me" |> should equal exp
    
[<Fact>]
let test_Primary_class () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=None}
    let exp = Some(Expression([[si]]), !!"me")
    pPrimary !!"(test) me" |> should equal exp

[<Fact>]
let test_Primary_dot () =
    pPrimary !!".\test me" |> should equal (Some(Primary.Dot, !!"est me"))
   
[<Fact>]
let test_SequenceItem () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=None}
    let exp = Some(si, !!"me")
    SequenceItem !!"test me" |> should equal exp
 
[<Fact>]
let test_SequenceItem_prefix_and_suffix () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=Some(Prefix.And);itemSuffix=Some(Suffix.Question)}
    let exp = Some(si, !!"me")
    SequenceItem !!"&test? me" |> should equal exp
    
[<Fact>]
let test_SequenceItem_prefix_only () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=Some(Prefix.Not);itemSuffix=None}
    let exp = Some(si, !!"me")
    SequenceItem !!"!test me" |> should equal exp    

[<Fact>]
let test_SequenceItem_suffix_only () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=Some(Suffix.Star)}
    let exp = Some(si, !!"me")
    SequenceItem !!"test* me" |> should equal exp    

[<Fact>]
let test_SequenceItem_suffix_only_2 () =
    let id = Primary.Identifier(!!"test")
    let si = {primaryItem=id;itemPrefix=None;itemSuffix=Some(Suffix.Plus)}
    let exp = Some(si, !!"me")
    SequenceItem !!"test+ me" |> should equal exp   
      
[<Fact>]  
let test_PEG_grammar () =
    let peg_grammar = !! @"

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

    let Some(defs, cl) = peg_grammar |> Grammar
    defs |> List.length |> should equal 29
    cl |> should equal []

[<Fact>]  
let test_PEG2_grammar () =
    let peg_grammar = !! @"

# Hierarchical syntax
grammar PEG {
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
LCURLY <- '{' Spacing #added left curly bracket
RCURLY <- '}' Spacing #added right curly bracket
DOT <- '.' Spacing

Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* EndOfLine
Space <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !.
}" 

    let Some(grammar, cl) = peg_grammar |> DevHawk.Parser.Peg2.Grammar
    grammar.name |> should equal !!"PEG"
    grammar.defs |> List.length |> should equal 31
    cl |> should equal []