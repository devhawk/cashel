#light

(*

# Hierarchical syntax
Grammar <- Spacing Identifier OCURLY Rule+ CCURLY EndOfFile
Rule <- Identifier LEFTARROW Production (SLASH Production)* SEMICOLON
Production <- PatternItem+ (RIGHTARROW Action)?
Suffix <- QUESTION / STAR / PLUS
Prefix <- AND / NOT / Identifier COLON
PatternItem <- Prefix? Primary Suffix?

#Identifier is fake right now
Action <- Identifier

#no longer can have multiple choices inside a parens. Instead, you can assoicate an action
Primary <- Identifier / OPEN Production CLOSE / Literal / Class / DOT

# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]

Literal <- ['] (!['] Char)* ['] Spacing / [""] (![""] Char)* [""] Spacing
Class <- '[' (!']' Range)* ']' Spacing
Range <- Char '-' Char / Char
#change char to unicode escape sequence \uxxxx
Char <- '\\' [nrt'""\[\]\\] / '\\u' HexDigit HexDigit HexDigit HexDigit / !'\\' .

HexDigit <- [a-fA-F0-9]

LEFTARROW <- '<-' Spacing
RIGHTARROW <- '=>' Spacing
COLON <- ':' ! Spacing
SLASH <- '/' Spacing
AND <- '&' Spacing
NOT <- '!' Spacing
QUESTION <- '?' Spacing
STAR <- '*' Spacing
PLUS <- '+' Spacing
OPEN <- '(' Spacing
CLOSE <- ')' Spacing
DOT <- '.' Spacing
OCURLY <- '{' Spacing
CCURLY <- '}' Spacing

Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* EndOfLine
Space <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !.

*)


namespace DevHawk.Parser

module Peg2

//---------------------------------------------------------------------------------------------
//Utility Functions

let List2String cl = 
    let sb = cl |> List.fold_left (fun (s:System.Text.StringBuilder) (c : char) -> s.Append(c)) (new System.Text.StringBuilder())
    sb.ToString()


//---------------------------------------------------------------------------------------------
//AST Types

type Range =
| Single of char
| Dual of char * char

type Arity =
| ZeroOrOne
| ZeroOrMore
| OneOrMore

type Prefix =
| SuccessPredicate
| FailurePredicate
| Variable of string

///Action type not defined yet. Using string as a stub
type Action = string //TBD


///A Primary can be an Identifier, Production list, 
type Primary =
| Identifier of string
| Production of Production 
| Literal of string
| Class of Range list
| Dot 

///A PatternItem is a Primary with an optional prefix and/or optional suffix
and PatternItem =
    { 
        item: Primary;
        prefix: Prefix option;     
        arity: Arity option;
    }

///A Production is a list of Pattern Items and an assoicated optional Action 
and Production = 
    {
        pattern: PatternItem list;
        action: Action option;
    }
    
///A Rule is a named list of Productions (in decending priority choice order)
type Rule = 
    {
        name: string;
        productions: Production list;
    }

///A Grammar is a named list of Rules
type Grammar = 
    {
        name: string;
        rules: Rule list;
    }

    
//---------------------------------------------------------------------------------------------

open DevHawk.Parser.Core
open DevHawk.Parser.Primitives
        
///EndOfLine  <- '\r\n' / '\n' / '\r'
let EndOfLine = parse {
    return! items_equal (List.of_seq "\r\n")
    return! item_equal '\n' |> listify
    return! item_equal '\r' |> listify }

///Space      <- ' ' / '\t' / EndOfLine
let Space = parse {
    return! item_equal ' ' |> listify
    return! item_equal '\t' |> listify
    return! EndOfLine }

///Comment    <- '#' (!EndOfLine .)* EndOfLine
let Comment = parse {
    do! skip_item '#' 
    let! c = repeat_until item EndOfLine
    return c }
          
///Spacing    <- (Space / Comment)*
let Spacing = parse {
    return! Space 
    return! Comment } |> repeat


///DOT        <- '.' Spacing
let DOT = item_equal '.' .>> Spacing

///OPEN       <- '(' Spacing
let OPAREN = item_equal '(' .>> Spacing

///CLOSE      <- ')' Spacing
let CPAREN = item_equal ')' .>> Spacing

///AND        <- '&' Spacing
let AND = item_equal '&' .>> Spacing

///NOT        <- '!' Spacing
let NOT = item_equal '!' .>> Spacing

///QUESTION   <- '?' Spacing
let QUESTION = item_equal '?' .>> Spacing

///STAR       <- '*' Spacing
let STAR = item_equal '*' .>> Spacing

///PLUS       <- '+' Spacing
let PLUS = item_equal '+' .>> Spacing 

///COLON       <- ':' Spacing
let COLON = item_equal ':' .>> Spacing 

///COLON       <- ':' Spacing
let SEMICOLON = item_equal ';' .>> Spacing 

///RIGHTARROW  <- '=>' Spacing
let RIGHTARROW = items_equal (List.of_seq "=>") .>> Spacing

///SLASH      <- '/' Spacing
let SLASH = item_equal '/' .>> Spacing

///OPEN       <- '(' Spacing
let OCURLY = item_equal '{' .>> Spacing

///CLOSE      <- ')' Spacing
let CCURLY = item_equal '}' .>> Spacing

///LEFTARROW  <- '<-' Spacing
let LEFTARROW = items_equal (List.of_seq "<-") .>> Spacing


///Char <- '\\' [nrt'""\[\]\\] / '\\u' [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] / !'\\' .
let _Char = 
    ///HexDigit <- [a-fA-F0-9]
    let HexDigit = any_of (['a'..'f'] @ ['A'..'F'] @  ['0'..'9'])

    let hex2int c = 
        let c = System.Char.ToUpper(c)
        if   System.Char.IsDigit(c) then Char.code c - Char.code '0'
        elif 'A' <= c && c <= 'F' then Char.code c - Char.code 'A' + 10 
        else failwith "Invalid Hex Digit"
    
    parse {
        do! skip_item '\\' 
        let! c = any_of ['n';'r';'t';'''; '"'; '['; ']'; '\\']
        match c with
        | 'n' -> return '\n'
        | 'r' -> return '\r'
        | 't' -> return '\t'
        | _ -> return c } 
    +++
    parse {        
        do! skip_item '\\' 
        do! skip_item 'u' 
        let! h1 = HexDigit
        let! h2 = HexDigit
        let! h3 = HexDigit
        let! h4 = HexDigit
        return Char.chr ((hex2int h1)*4096 + (hex2int h2)*256 + (hex2int h3)*16 + (hex2int h4)) }
    +++ 
    parse {
        do! !~ (item_equal '\\')
        return! item }
    
///Range      <- Char '-' Char / Char
let _Range =
    parse {
        let! c1 = _Char
        do! skip_item '-' 
        let! c2 = _Char
        return Dual(c1, c2) }
    +++
    parse {
        let! c1 = _Char
        return Single(c1) }
     
///Class      <- '[' (!']' Range)* ']' Spacing
let _Class =
    parse {
        do! skip_item '['
        let! rl = repeat_until _Range (item_equal ']')
        do! ignore Spacing 
        return rl
    }
           
///Literal    <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* [""] Spacing    
let _Literal =
    let literal_workhorse ch = 
        parse {
            do! skip_item ch
            let! cl = repeat_until _Char (item_equal ch)
            do! ignore Spacing 
            return List2String cl }
    literal_workhorse ''' +++ literal_workhorse '"'
    

///Identifier <- [a-zA-Z_] ([_a-zA-Z0-9])* Spacing
let _Identifier =
    parse {
        let! c = any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'])
        let! cs = repeat (any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']))
        do! ignore Spacing 
        return List2String (c::cs) }

//Stub out Action for now    
let _Action = _Identifier

///Primary <- Identifier !LEFTARROW  / OPEN Production CLOSE / Literal / Class / DOT
//Had to name this method pPrimary to avoid conflict with Primary discriminated union
let rec _Primary =
    parse {
        let! id = _Identifier
        return Identifier(id) }
    +++ 
    parse {
        do! ignore OPAREN 
        let! prod = _Production
        do! ignore CPAREN 
        return Production(prod) }        
    +++ 
    parse {
        let! lit = _Literal
        return Literal(lit) }        
    +++ 
    parse {
        let! cls = _Class
        return Class(cls) }
    +++ 
    (DOT >>$ Primary.Dot)


///Suffix <- QUESTION / STAR / PLUS
///Prefix <- AND / NOT / Identifier COLON
///PatternItem <- Prefix? Primary Suffix?
and _PatternItem =
    let _Prefix = (AND >>$ SuccessPredicate) +++ (NOT >>$ FailurePredicate) +++ (_Identifier >>= (fun id -> COLON >>$ Variable(id)))
    let _Arity = (QUESTION >>$ ZeroOrOne) +++ (STAR >>$ ZeroOrMore) +++ (PLUS >>$ OneOrMore)
    parse {
        let! pre = !? _Prefix
        let! pri = _Primary
        let! suf = !? _Arity
        return {item=pri;prefix=pre;arity=suf} }

///Production <- PatternItem+ (RIGHTARROW Action)?
and _Production = 
    parse {
        let! pl = repeat1 _PatternItem
        let! a = !? (RIGHTARROW >>. _Action)
        return {pattern=pl; action=a}  }

///Rule <- Identifier LEFTARROW Production (SLASH Production)* SEMICOLON
let _Rule =
    parse {
        let! id = _Identifier
        do! ignore LEFTARROW
        let! p = _Production
        let! pl = repeat (SLASH >>. _Production)
        do! ignore SEMICOLON
        return {name=id;productions=p::pl} }

///Grammar <- Spacing Identifier OCURLY Rule+ CCURLY EndOfFile
let _Grammar =
    parse {
        do! ignore Spacing
        let! id = _Identifier
        do! ignore OCURLY
        let! rl = repeat1 _Rule
        do! ignore CCURLY
        do! eof
        return {name=id; rules=rl} }


