#nowarn "40"
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


namespace Cashel.Sample

module Peg2 =
    
    //---------------------------------------------------------------------------------------------
    //Utility Functions
    
    let List2String cl = 
        let sb = cl |> List.fold (fun (s:System.Text.StringBuilder) (c : char) -> s.Append(c)) (new System.Text.StringBuilder())
        sb.ToString()
    
    let ListFoldString l = l |> List.fold (fun s p -> sprintf "%s %O" s p) ""
    
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
    let replacews (s:string) = s.Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t")
    
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
            | Literal s -> sb |> appendChar '\"' |> append (replacews s) |> appendChar '\"'
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
    
    open Cashel
    open Cashel.ListPrimitives
            
    let _EndOfFile = !~ token
    
    ///EndOfLine  <- '\r\n' / '\n' / '\r'
    let _EndOfLine = parser {
        return! matchTokens (List.ofSeq "\r\n")
        return! matchToken '\n' |> listify
        return! matchToken '\r' |> listify } |> forget
    
    ///Space      <- ' ' / '\t' / EndOfLine
    let _Space = parser {
        return! matchToken ' ' |> forget
        return! matchToken '\t' |> forget
        return! _EndOfLine } |> forget
    
    ///SlashComment    <- '//' (!EndOfLine .)* EndOfLine
    let _SlashComment = parser {
        do! skips ['/';'/']
        do! until token _EndOfLine |> forget
        return () } 
        
    ///Comment    <- '#' (!EndOfLine .)* EndOfLine
    let _Comment = parser {
        do! skip '#' 
        do! until token _EndOfLine |> forget
        return () } 
              
    ///Spacing    <- (Space / Comment)*
    let _Spacing = forget (parser {
        return! _Space 
        return! _SlashComment
        return! _Comment } |> repeat)
    
    let parse p = _Spacing >>. p
    let forgottenToken p = forget (p .>> _Spacing)
    
    ///DOT        <- '.' Spacing
    let _DOT = forgottenToken (matchToken '.')
    
    ///OPEN       <- '(' Spacing
    let _OPAREN = forgottenToken (matchToken '(')
    
    ///CLOSE      <- ')' Spacing
    let _CPAREN = forgottenToken (matchToken ')')
    
    ///AND        <- '&' Spacing
    let _AND = forgottenToken (matchToken '&')
    
    ///NOT        <- '!' Spacing
    let _NOT = forgottenToken (matchToken '!')
    
    ///QUESTION   <- '?' Spacing
    let _QUESTION = forgottenToken (matchToken '?')
    
    ///STAR       <- '*' Spacing
    let _STAR = forgottenToken (matchToken '*')
    
    ///PLUS       <- '+' Spacing
    let _PLUS = forgottenToken (matchToken '+')
    
    ///COLON       <- ':' Spacing
    let _COLON = forgottenToken (matchToken ':')
    
    ///SEMICOLON       <- ';' Spacing
    let _SEMICOLON = forgottenToken (matchToken ';')
    
    ///RIGHTARROW  <- '=>' Spacing
    let _RIGHTARROW = forgottenToken (matchTokens (List.ofSeq "=>"))
    
    ///SLASH      <- '/' Spacing
    let _SLASH = forgottenToken (matchToken '/')
    
    ///OPEN       <- '(' Spacing
    let _OCURLY = forgottenToken (matchToken '{')
    
    ///CLOSE      <- ')' Spacing
    let _CCURLY = forgottenToken (matchToken '}')
    
    ///LEFTARROW  <- '<-' Spacing
    let _LEFTARROW = forgottenToken (matchTokens (List.ofSeq "<-"))
    
    
    ///Char <- '\\' [nrt'""\[\]\\] / '\\u' [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] / !'\\' .
    let _Char = 
        ///HexDigit <- [a-fA-F0-9]
        let _HexDigit = any (['a'..'f'] @ ['A'..'F'] @  ['0'..'9'])
    
        let hex2int c = 
            let c = System.Char.ToUpper(c)
            if   System.Char.IsDigit(c) then int c - int '0'
            elif 'A' <= c && c <= 'F' then int c - int 'A' + 10 
            else failwith "Invalid Hex Digit"
        
        parser {
            do! skip '\\' 
            let! c = any ['n';'r';'t';'''; '"'; '['; ']'; '\\']
            match c with
            | 'n' -> return '\n'
            | 'r' -> return '\r'
            | 't' -> return '\t'
            | _ -> return c } 
        +++
        parser {        
            do! skip '\\' 
            do! skip 'u' 
            let! h1 = _HexDigit
            let! h2 = _HexDigit
            let! h3 = _HexDigit
            let! h4 = _HexDigit
            return char ((hex2int h1)*4096 + (hex2int h2)*256 + (hex2int h3)*16 + (hex2int h4)) }
        +++ 
        parser {
            do! !~ (matchToken '\\')
            return! token }
        
    ///Range      <- Char '-' Char / Char
    let _Range =
        parser {
            let! c1 = _Char
            do! skip '-' 
            let! c2 = _Char
            return Dual(c1, c2) }
        +++
        parser {
            let! c1 = _Char
            return Single(c1) }
    
    ///Class      <- '[' (!']' Range)* ']' Spacing
    let _Class =
        parser {
            do! skip '['
            let! rl = until _Range (matchToken ']')
            do! forget _Spacing 
            return rl
        }
               
    ///Literal    <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* [""] Spacing    
    let _Literal =
        let literalWorkhorse ch = 
            parser {
                do! skip ch
                let! cl = until _Char (matchToken ch)
                do! forget _Spacing 
                return List2String cl }
        literalWorkhorse ''' +++ literalWorkhorse '"'
        
    
    ///Identifier <- [a-zA-Z_] ([_a-zA-Z0-9])* Spacing
    let _Identifier =
        parser {
            let! c = any (['_'] @ ['a'..'z'] @ ['A'..'Z'])
            let! cs = repeat (any (['_'] @ ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']))
            do! forget _Spacing 
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
            do! forget _OPAREN 
            let! prod = _Production
            do! forget _CPAREN 
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
        (_DOT >>! Dot )
    
    
    ///Suffix <- QUESTION / STAR / PLUS
    ///Prefix <- AND / NOT / Identifier COLON
    ///PatternItem <- Prefix? Primary Suffix?
    and _PatternItem =
        let _Prefix = 
            (_AND >>! SuccessPredicate)
            +++ 
            (_NOT >>! FailurePredicate)
            +++ 
            parser {
                let! id = _Identifier
                do! _COLON |> forget
                return Variable(id) }
                
        let _Arity = 
            (_QUESTION >>! ZeroOrOne)
            +++ 
            (_STAR >>! ZeroOrMore)
            +++ 
            (_PLUS >>! OneOrMore)
    
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
            do! forget _LEFTARROW
            let! p = _Production
            let! pl = repeat (_SLASH >>. _Production)
            do! forget _SEMICOLON
            return {name=id;productions=p::pl} }
    
    ///Grammar <- Spacing Identifier OCURLY Rule+ CCURLY EndOfFile
    let _Grammar =
        parser {
            do! forget _Spacing
            let! id = _Identifier
            do! _OCURLY
            let! rl = repeat1 _Rule
            do! _CCURLY
            do! _EndOfFile
            return {name=id; rules=rl} }
    
    let Parse (input:string) =
        let g = _Grammar (List.ofSeq input)
        match g with 
        | Some(g, []) -> Some(g)
        | _ -> None
        
    
    
    