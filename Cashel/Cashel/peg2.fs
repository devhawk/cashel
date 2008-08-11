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

//---------------------------------------------------------------------------------------------
//AST Types

type Range =
| Single of char
| Dual of char * char
    with
    override this.ToString() = 
        match this with
        | Single x -> sprintf "Range.Single (%c)" x
        | Dual (x,y) -> sprintf "Range.Dual (%c,%c)" x y
        
type Arity =
| ZeroOrOne
| ZeroOrMore
| OneOrMore
    with
    override this.ToString() = 
        match this with
        | ZeroOrOne -> sprintf "Arity.ZeroOrOne" 
        | ZeroOrMore -> sprintf "Arity.ZeroOrMore"
        | OneOrMore -> sprintf "Arity.OneOrMore"
        
type Prefix =
| SuccessPredicate
| FailurePredicate
| Variable of string
    with
    override this.ToString() = 
        match this with
        | SuccessPredicate -> sprintf "Prefix.Success" 
        | FailurePredicate -> sprintf "Prefix.Failure"
        | Variable s -> sprintf "Prefix.Var (%s)" s
        
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
    override this.ToString() = 
        match this with
        | Identifier s -> sprintf "Primary.Identifier(%s)" s
        | Production p -> sprintf "Primary.Production %O" p
        | Literal s -> sprintf "Primary.Literal(\"%s\")" s
        | Class rl ->  sprintf "Primary.Class(%s)" (ListFoldString rl)
        | Dot -> sprintf "Primary.Dot"

///A PatternItem is a Primary with an optional prefix and/or optional suffix
and PatternItem =
    { 
        item: Primary;
        prefix: Prefix option;     
        arity: Arity option;
    }
    with 
    override this.ToString() = 
        let sb = new System.Text.StringBuilder("PatternItem (")
        if Option.is_some this.prefix then 
            sb.AppendFormat("{0} ", (Option.get this.prefix)) |> ignore
        sb.Append(this.item) |> ignore
        if Option.is_some this.arity then 
            sb.AppendFormat(" {0}", (Option.get this.arity)) |> ignore
        sb.Append(')') |> ignore
        sb.ToString()
        
///A Production is a list of Pattern Items and an assoicated optional Action 
and Production = 
    {
        pattern: PatternItem list;
        action: Action option;
    }
    with 
    override this.ToString() = 
        let pil = ListFoldString this.pattern
        if Option.is_some this.action 
            then sprintf "Production (Pattern: %s, Action: %s)" pil (Option.get this.action)
            else sprintf "Production (Pattern: %s)" pil 
    
///A Rule is a named list of Productions (in decending priority choice order)
type Rule = 
    {
        name: string;
        productions: Production list;
    }
    with 
    override this.ToString() = sprintf "Rule \"%s\" (%s)" this.name (ListFoldString this.productions)

///A Grammar is a named list of Rules
type Grammar = 
    {
        name: string;
        rules: Rule list;
    }
    with 
    override this.ToString() = sprintf "Grammar \"%s\" (%s)" this.name (ListFoldString this.rules)

    
//---------------------------------------------------------------------------------------------

open DevHawk.Parser.Core
open DevHawk.Parser.Primitives
        
///EndOfLine  <- '\r\n' / '\n' / '\r'
let _EndOfLine = parse {
    return! items_equal (List.of_seq "\r\n")
    return! item_equal '\n' |> listify
    return! item_equal '\r' |> listify }

///Space      <- ' ' / '\t' / EndOfLine
let _Space = parse {
    return! item_equal ' ' |> listify
    return! item_equal '\t' |> listify
    return! _EndOfLine }

///Comment    <- '#' (!EndOfLine .)* EndOfLine
let _Comment = parse {
    do! skip_item '#' 
    let! c = repeat_until item _EndOfLine
    return c }
          
///Spacing    <- (Space / Comment)*
let _Spacing = parse {
    return! _Space 
    return! _Comment } |> repeat


///DOT        <- '.' Spacing
let _DOT = item_equal '.' .>> _Spacing

///OPEN       <- '(' Spacing
let _OPAREN = item_equal '(' .>> _Spacing

///CLOSE      <- ')' Spacing
let _CPAREN = item_equal ')' .>> _Spacing

///AND        <- '&' Spacing
let _AND = item_equal '&' .>> _Spacing

///NOT        <- '!' Spacing
let _NOT = item_equal '!' .>> _Spacing

///QUESTION   <- '?' Spacing
let _QUESTION = item_equal '?' .>> _Spacing

///STAR       <- '*' Spacing
let _STAR = item_equal '*' .>> _Spacing

///PLUS       <- '+' Spacing
let _PLUS = item_equal '+' .>> _Spacing 

///COLON       <- ':' Spacing
let _COLON = item_equal ':' .>> _Spacing 

///COLON       <- ':' Spacing
let _SEMICOLON = item_equal ';' .>> _Spacing 

///RIGHTARROW  <- '=>' Spacing
let _RIGHTARROW = items_equal (List.of_seq "=>") .>> _Spacing

///SLASH      <- '/' Spacing
let _SLASH = item_equal '/' .>> _Spacing

///OPEN       <- '(' Spacing
let _OCURLY = item_equal '{' .>> _Spacing

///CLOSE      <- ')' Spacing
let _CCURLY = item_equal '}' .>> _Spacing

///LEFTARROW  <- '<-' Spacing
let _LEFTARROW = items_equal (List.of_seq "<-") .>> _Spacing


///Char <- '\\' [nrt'""\[\]\\] / '\\u' [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] / !'\\' .
let _Char = 
    ///HexDigit <- [a-fA-F0-9]
    let _HexDigit = any_of (['a'..'f'] @ ['A'..'F'] @  ['0'..'9'])

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
        let! h1 = _HexDigit
        let! h2 = _HexDigit
        let! h3 = _HexDigit
        let! h4 = _HexDigit
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
        do! ignore _Spacing 
        return rl
    }
           
///Literal    <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* [""] Spacing    
let _Literal =
    let literal_workhorse ch = 
        parse {
            do! skip_item ch
            let! cl = repeat_until _Char (item_equal ch)
            do! ignore _Spacing 
            return List2String cl }
    literal_workhorse ''' +++ literal_workhorse '"'
    

///Identifier <- [a-zA-Z_] ([_a-zA-Z0-9])* Spacing
let _Identifier =
    parse {
        let! c = any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'])
        let! cs = repeat (any_of (['_'] @ ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']))
        do! ignore _Spacing 
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
        do! ignore _OPAREN 
        let! prod = _Production
        do! ignore _CPAREN 
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
        parse {
            let! id = _Identifier
            do! _COLON |> ignore
            return Variable(id) }
            
    let _Arity = 
        (_QUESTION >>$ ZeroOrOne)
        +++ 
        (_STAR >>$ ZeroOrMore)
        +++ 
        (_PLUS >>$ OneOrMore)

    parse {
        let! pre = !? _Prefix
        let! pri = _Primary
        let! suf = !? _Arity
        return {item=pri;prefix=pre;arity=suf} }

///Production <- PatternItem+ (RIGHTARROW Action)?
and _Production = 
    parse {
        let! pl = repeat1 _PatternItem
        let! a = !? (_RIGHTARROW >>. _Action)
        return {pattern=pl; action=a}  }

///Rule <- Identifier LEFTARROW Production (SLASH Production)* SEMICOLON
let _Rule =
    parse {
        let! id = _Identifier
        do! ignore _LEFTARROW
        let! p = _Production
        let! pl = repeat (_SLASH >>. _Production)
        do! ignore _SEMICOLON
        return {name=id;productions=p::pl} }

///Grammar <- Spacing Identifier OCURLY Rule+ CCURLY EndOfFile
let _Grammar =
    parse {
        do! ignore _Spacing
        let! id = _Identifier
        do! ignore _OCURLY
        let! rl = repeat1 _Rule
        do! ignore _CCURLY
        do! eof
        return {name=id; rules=rl} }

let Parse (input:string) =
    let g = _Grammar (List.of_seq input)
    match g with 
    | Some(g, []) -> Some(g)
    | _ -> None
