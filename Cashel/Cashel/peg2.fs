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

let ListFoldString l = l |> List.fold_left (fun s p -> sprintf "%s %O" s p) ""

type StrBuilder = System.Text.StringBuilder
let append (txt:string) (sb:StrBuilder) = sb.Append(txt)
let appendChar (c:char) (sb:StrBuilder) = sb.Append(c)
let appendList l func sb = 
    l |> List.iter (fun x -> sb |> func x |> ignore)
    sb
let appendOption v func sb =
    match v with
    | Some(v) -> sb |> func v 
    | None -> sb
    
let tostr o = o.ToString()
let replace_ws (s:string) = s.Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t")

//---------------------------------------------------------------------------------------------
//AST Types

type Range =
| Single of char
| Dual of char * char
    with
    override this.ToString() = new StrBuilder() |> Range.BuildString this |> tostr
    static member BuildString (r:Range) (sb:StrBuilder) = 
        match r with
        | Single x -> sb |> appendChar x 
        | Dual (x,y) -> sb |> appendChar x |> appendChar '-' |> appendChar y 
        
type Arity =
| ZeroOrOne
| ZeroOrMore
| OneOrMore
    with
    override this.ToString() = new StrBuilder() |> Arity.BuildString this |> tostr
    static member BuildString (a:Arity) (sb:StrBuilder) = 
        match a with
        | ZeroOrOne -> sb |> appendChar '?' 
        | ZeroOrMore -> sb |> appendChar '*'
        | OneOrMore -> sb |> appendChar '+'
        
type Prefix =
| SuccessPredicate
| FailurePredicate
| Variable of string
    with
    override this.ToString() = new StrBuilder() |> Prefix.BuildString this |> tostr
    static member BuildString (p:Prefix) (sb:StrBuilder) = 
        match p with
        | SuccessPredicate -> sb |> appendChar '&' 
        | FailurePredicate -> sb |> appendChar '!' 
        | Variable s -> sb |> append s |> appendChar ':' 
        
///Action type not defined yet. Using string as a stub
type Action = string //TBD

///A Primary can be an Identifier, Production list, 
type Primary =
| Identifier of string
| Production of Production 
| Literal of string
| Class of Range list
| Dot 
    with
    override this.ToString() = new StrBuilder() |> Primary.BuildString this |> tostr
    static member BuildString (p:Primary) (sb:StrBuilder) = 
        match p with
        | Identifier s -> sb |> append s
        | Production p -> sb |> appendChar '(' |> Production.BuildString p |> appendChar ')'
        | Literal s -> sb |> appendChar '\"' |> append (replace_ws s) |> appendChar '\"'
        | Class rl ->  sb |> appendChar '[' |> appendList rl Range.BuildString |> appendChar ']'
        | Dot -> sb |> append "."
    
///A PatternItem is a Primary with an optional prefix and/or optional suffix
and PatternItem =
    { 
        item: Primary;
        prefix: Prefix option;     
        arity: Arity option;
    }
    with 
    override this.ToString() = new StrBuilder() |> PatternItem.BuildString this |> tostr
    static member BuildString (p:PatternItem) (sb:StrBuilder) = 
        sb |> appendOption p.prefix Prefix.BuildString |> Primary.BuildString p.item |> appendOption p.arity Arity.BuildString
        
    
///A Production is a list of Pattern Items and an assoicated optional Action 
and Production = 
    {
        pattern: PatternItem list;
        action: Action option;
    }
    with 
    override this.ToString() = new StrBuilder() |> Production.BuildString this |> tostr
    static member BuildString (prod:Production) (sb:StrBuilder) = 
        sb |> appendList prod.pattern (fun pi sb -> sb |> PatternItem.BuildString pi |> appendChar ' ') |> appendOption prod.action (fun a sb -> sb |> append "=> " |> append a)
    
///A Rule is a named list of Productions (in decending priority choice order)
type Rule = 
    {
        name: string;
        productions: Production list;
    }
    with 
    override this.ToString() = new StrBuilder() |> Rule.BuildString this |> tostr
    static member BuildString (r:Rule) (sb:StrBuilder) = 
        let appendProductions sb = 
            match r.productions with
            | hd::tl -> 
                sb |> Production.BuildString hd |> appendList tl (fun p sb -> sb |> append " /\n\t\t" |> Production.BuildString p |> ignore) 
            | _ -> sb
        sb |> append "\t" |> append r.name |> append " <- " |> appendProductions |> append ";\n"  

             
///A Grammar is a named list of Rules
type Grammar = 
    {
        name: string;
        rules: Rule list;
    }
    with 
    override this.ToString() = new StrBuilder() |> Grammar.BuildString this |> tostr
    static member BuildString (g:Grammar) (sb:StrBuilder) = 
        sb |> append g.name |> append "\n{\n" |> appendList g.rules Rule.BuildString |> append "\n}\n" 

        

    
//---------------------------------------------------------------------------------------------

open DevHawk.Parser.Core
open DevHawk.Parser.Primitives
        
let _EndOfFile = !~ item

///EndOfLine  <- '\r\n' / '\n' / '\r'
let _EndOfLine = parser {
    return! items_equal (List.of_seq "\r\n")
    return! item_equal '\n' |> listify
    return! item_equal '\r' |> listify } |> ignore

///Space      <- ' ' / '\t' / EndOfLine
let _Space = parser {
    return! item_equal ' ' |> ignore
    return! item_equal '\t' |> ignore
    return! _EndOfLine } |> ignore

///SlashComment    <- '//' (!EndOfLine .)* EndOfLine
let _SlashComment = parser {
    do! skip_items ['/';'/']
    do! repeat_until item _EndOfLine |> ignore
    return () } 
    
///Comment    <- '#' (!EndOfLine .)* EndOfLine
let _Comment = parser {
    do! skip_item '#' 
    do! repeat_until item _EndOfLine |> ignore
    return () } 
          
///Spacing    <- (Space / Comment)*
let _Spacing = ignore (parser {
    return! _Space 
    return! _SlashComment
    return! _Comment } |> repeat)

let parse p = _Spacing >>. p
let token p = ignore (p .>> _Spacing)

///DOT        <- '.' Spacing
let _DOT = token (item_equal '.')

///OPEN       <- '(' Spacing
let _OPAREN = token (item_equal '(')

///CLOSE      <- ')' Spacing
let _CPAREN = token (item_equal ')')

///AND        <- '&' Spacing
let _AND = token (item_equal '&')

///NOT        <- '!' Spacing
let _NOT = token (item_equal '!')

///QUESTION   <- '?' Spacing
let _QUESTION = token (item_equal '?')

///STAR       <- '*' Spacing
let _STAR = token (item_equal '*')

///PLUS       <- '+' Spacing
let _PLUS = token (item_equal '+')

///COLON       <- ':' Spacing
let _COLON = token (item_equal ':')

///SEMICOLON       <- ';' Spacing
let _SEMICOLON = token (item_equal ';')

///RIGHTARROW  <- '=>' Spacing
let _RIGHTARROW = token (items_equal (List.of_seq "=>"))

///SLASH      <- '/' Spacing
let _SLASH = token (item_equal '/')

///OPEN       <- '(' Spacing
let _OCURLY = token (item_equal '{')

///CLOSE      <- ')' Spacing
let _CCURLY = token (item_equal '}')

///LEFTARROW  <- '<-' Spacing
let _LEFTARROW = token (items_equal (List.of_seq "<-"))


///Char <- '\\' [nrt'""\[\]\\] / '\\u' [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] / !'\\' .
let _Char = 
    ///HexDigit <- [a-fA-F0-9]
    let _HexDigit = any_of (['a'..'f'] @ ['A'..'F'] @  ['0'..'9'])

    let hex2int c = 
        let c = System.Char.ToUpper(c)
        if   System.Char.IsDigit(c) then int c - int '0'
        elif 'A' <= c && c <= 'F' then int c - int 'A' + 10 
        else failwith "Invalid Hex Digit"
    
    parser {
        do! skip_item '\\' 
        let! c = any_of ['n';'r';'t';'''; '"'; '['; ']'; '\\']
        match c with
        | 'n' -> return '\n'
        | 'r' -> return '\r'
        | 't' -> return '\t'
        | _ -> return c } 
    +++
    parser {        
        do! skip_item '\\' 
        do! skip_item 'u' 
        let! h1 = _HexDigit
        let! h2 = _HexDigit
        let! h3 = _HexDigit
        let! h4 = _HexDigit
        return char ((hex2int h1)*4096 + (hex2int h2)*256 + (hex2int h3)*16 + (hex2int h4)) }
    +++ 
    parser {
        do! !~ (item_equal '\\')
        return! item }
    
///Range      <- Char '-' Char / Char
let _Range =
    parser {
        let! c1 = _Char
        do! skip_item '-' 
        let! c2 = _Char
        return Dual(c1, c2) }
    +++
    parser {
        let! c1 = _Char
        return Single(c1) }

///Class      <- '[' (!']' Range)* ']' Spacing
let _Class =
    parser {
        do! skip_item '['
        let! rl = repeat_until _Range (item_equal ']')
        do! ignore _Spacing 
        return rl
    }
           
///Literal    <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* [""] Spacing    
let _Literal =
    let literal_workhorse ch = 
        parser {
            do! skip_item ch
            let! cl = repeat_until _Char (item_equal ch)
            do! ignore _Spacing 
            return List2String cl }
    literal_workhorse ''' +++ literal_workhorse '"'
    

///Identifier <- [a-zA-Z_] ([_a-zA-Z0-9])* Spacing
let _Identifier =
    parser {
        let! c = any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'])
        let! cs = repeat (any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']))
        do! ignore _Spacing 
        return List2String (c::cs) }

//Stub out Action for now    
let _Action = _Identifier

///Primary <- Identifier / OPEN Production CLOSE / Literal / Class / DOT
//Had to name this method pPrimary to avoid conflict with Primary discriminated union
let rec _Primary =
    parser {
        let! id = _Identifier
        return Identifier(id) }
    +++ 
    parser {
        do! ignore _OPAREN 
        let! prod = _Production
        do! ignore _CPAREN 
        return Production(prod) }        
    +++ 
    parser {
        let! lit = _Literal
        return Literal(lit) }        
    +++ 
    parser {
        let! cls = _Class
        return Class(cls) }
    +++ 
    (_DOT >>$ Dot )


///Suffix <- QUESTION / STAR / PLUS
///Prefix <- AND / NOT / Identifier COLON
///PatternItem <- Prefix? Primary Suffix?
and _PatternItem =
    let _Prefix = 
        (_AND >>$ SuccessPredicate)
        +++ 
        (_NOT >>$ FailurePredicate)
        +++ 
        parser {
            let! id = _Identifier
            do! _COLON |> ignore
            return Variable(id) }
            
    let _Arity = 
        (_QUESTION >>$ ZeroOrOne)
        +++ 
        (_STAR >>$ ZeroOrMore)
        +++ 
        (_PLUS >>$ OneOrMore)

    parser {
        let! pre = !? _Prefix
        let! pri = _Primary
        let! suf = !? _Arity
        return {item=pri;prefix=pre;arity=suf} }

///Production <- PatternItem+ (RIGHTARROW Action)?
and _Production = 
    parser {
        let! pl = repeat1 _PatternItem
        let! a = !? (_RIGHTARROW >>. _Action)
        return {pattern=pl; action=a}  }

///Rule <- Identifier LEFTARROW Production (SLASH Production)* SEMICOLON
let _Rule =
    parser {
        let! id = _Identifier
        do! ignore _LEFTARROW
        let! p = _Production
        let! pl = repeat (_SLASH >>. _Production)
        do! ignore _SEMICOLON
        return {name=id;productions=p::pl} }

///Grammar <- Spacing Identifier OCURLY Rule+ CCURLY EndOfFile
let _Grammar =
    parser {
        do! ignore _Spacing
        let! id = _Identifier
        do! _OCURLY
        let! rl = repeat1 _Rule
        do! _CCURLY
        do! _EndOfFile
        return {name=id; rules=rl} }

let Parse (input:string) =
    let g = _Grammar (List.of_seq input)
    match g with 
    | Some(g, []) -> Some(g)
    | _ -> None
    


