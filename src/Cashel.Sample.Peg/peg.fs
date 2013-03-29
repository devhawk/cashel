#nowarn "40"
(*

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
EndOfFile <- !.

*)


namespace Cashel.Sample

module Peg =
    
    //---------------------------------------------------------------------------------------------
    //AST Types
    
    let rec L2S (input : char list) = 
        new System.String(List.toArray input)
        
    ///AST Type for Range production
    type Range =
    | Single of char
    | Dual of char * char
        with
        override this.ToString() = 
            match this with
            | Single x -> sprintf "Range.Single (%A)" x
            | Dual (x,y) -> sprintf "Range.Dual (%A,%A)" x y
    
    ///AST Type for Suffix production
    type Suffix =
    | Question
    | Star
    | Plus
        with
        override this.ToString() = 
            match this with
            | Question -> "Suffix.Question"
            | Star -> "Suffix.Star"
            | Plus -> "Suffix.Plus"
    
    ///AST Type for Prefix production
    type Prefix =
    | And
    | Not
        with
        override this.ToString() = 
            match this with
            | And -> "Prefix.And"
            | Not -> "Prefix.Not"
    
    ///AST Type for Primary production
    type Primary =
    | Identifier of char list
    | Expression of Expression
    | Literal of char list
    | Class of Range list
    | Dot 
        with
        static member Exp2Str (exp : Expression) =
            let sb = new System.Text.StringBuilder()
            for sequence in exp do
                sb.Append(" Sequence") |> ignore
                for si in sequence do
                    sb.AppendFormat(" {0}", si) |> ignore
            sb.ToString()
        override this.ToString() = 
            match this with
            | Identifier i -> sprintf "Primary.Identifier %s" (L2S i)
            | Expression e -> sprintf "Primary.Expression %s" (Primary.Exp2Str e)
            | Literal l -> sprintf "Primary.Literal %s" (L2S l)
            | Class rl -> 
                let sb = new System.Text.StringBuilder("Primary.Class ")
                for r in rl do
                    sb.AppendFormat("{0}, ", r) |> ignore
                sb.ToString()
            | Dot -> "Primary.Dot"
    
    ///AST Type for Sequence Item production
    and SequenceItem =
        { 
            primaryItem: Primary;
            itemPrefix: Prefix option;     
            itemSuffix: Suffix option;
        }
        with 
        override this.ToString() = 
            let sb = new System.Text.StringBuilder("SequenceItem ")
            if Option.isSome this.itemPrefix then 
                sb.AppendFormat("{0} ", (Option.get this.itemPrefix)) |> ignore
            sb.Append(this.primaryItem) |> ignore
            if Option.isSome this.itemSuffix then 
                sb.AppendFormat(" {0}", (Option.get this.itemSuffix)) |> ignore
            sb.ToString()
    
    and Sequence = SequenceItem list
    
    and Expression = Sequence list
    
    ///AST Type for Definition production
    type Definition = 
        {
            name: char list;
            exp: Expression;
        }
        with
        override this.ToString() = 
            sprintf "Definition (name: %A, exp: %A)" (L2S this.name) (Primary.Exp2Str this.exp)
    
    //---------------------------------------------------------------------------------------------
    
    open Cashel
    open Cashel.ListPrimitives
    
    ///EndOfFile  <- !. 
    let EndOfFile = eof
            
    ///EndOfLine  <- '\r\n' / '\n' / '\r'
    let EndOfLine = parser {
        return! matchTokens (List.ofSeq "\r\n")
        return! matchToken '\n' |> listify
        return! matchToken '\r' |> listify }
    
    ///Space      <- ' ' / '\t' / EndOfLine
    let Space = parser {
        return! matchToken ' ' |> listify
        return! matchToken '\t' |> listify
        return! EndOfLine }
    
    ///Comment    <- '#' (!EndOfLine .)* EndOfLine
    let Comment = parser {
        do! skip '#' 
        let! c = until token EndOfLine
        return c }
              
    ///Spacing    <- (Space / Comment)*
    let Spacing = parser {
        return! Space 
        return! Comment } |> repeat
    
    ///DOT        <- '.' Spacing
    let DOT = matchToken '.' .>> Spacing
    
    ///SLASH      <- '/' Spacing
    let SLASH = matchToken '/' .>> Spacing
    
    ///AND        <- '&' Spacing
    let AND = matchToken '&' .>> Spacing
    
    ///NOT        <- '!' Spacing
    let NOT = matchToken '!' .>> Spacing
    
    ///QUESTION   <- '?' Spacing
    let QUESTION = matchToken '?' .>> Spacing
    
    ///STAR       <- '*' Spacing
    let STAR = matchToken '*' .>> Spacing
    
    ///PLUS       <- '+' Spacing
    let PLUS = matchToken '+' .>> Spacing
    
    ///OPEN       <- '(' Spacing
    let OPEN = matchToken '(' .>> Spacing
    
    ///CLOSE      <- ')' Spacing
    let CLOSE = matchToken ')' .>> Spacing
    
    ///LEFTARROW  <- '<-' Spacing
    let LEFTARROW = matchTokens (List.ofSeq "<-") .>> Spacing
    
    ///Char <- '\\' [nrt'""\[\]\\] / '\\' [0-2][0-7][0-7] / '\\' [0-7][0-7]? / !'\\' .
    let Char = 
        let c2i c = int c - int '0'
        
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
            let! c1 = any ['0'..'2'] 
            let! c2 = any ['0'..'7']
            let! c3 = any ['0'..'7']
            return char ((c2i c1)*64 + (c2i c2)*8 + (c2i c3)) }
        +++
        parser {        
            do! skip '\\' 
            let! c1 = any ['0'..'7']
            let! c2 = !? (any ['0'..'7'])
            match c2 with
            | Some(c2) -> return char ((c2i c1)*8 + (c2i c2))
            | None -> return char (c2i c1) } 
        +++ 
        parser {
            do! !~ (matchToken '\\')
            return! token }
        
    ///Range      <- Char '-' Char / Char
    let Range =
        parser {
            let! c1 = Char
            do! skip '-' 
            let! c2 = Char
            return Dual(c1, c2) }
        +++
        parser {
            let! c1 = Char
            return Single(c1) }
         
    ///Class      <- '[' (!']' Range)* ']' Spacing
    let Class =
        parser {
            do! skip '['
            let! rl = until Range (matchToken ']')
            do! forget Spacing 
            return rl
        }
               
    ///Literal    <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* [""] Spacing    
    let Literal =
        let literal_workhorse ch = parser {
            do! skip ch
            let! cl = until Char (matchToken ch)
            do! forget Spacing 
            return cl }
        literal_workhorse ''' +++ literal_workhorse '"'
        
    
    ///Identifier <- IdentStart IdentCont* Spacing
    //IdentStart <- [a-zA-Z_]
    //IdentCont  <- IdentStart / [0-9]            
    let Identifier =
        let IdentStart = any (['a'..'z']@['A'..'Z']@['_'])
        let IdentCont = IdentStart +++ any ['0'..'9']
    
        parser {
            let! c = IdentStart
            let! cs = repeat IdentCont 
            do! forget Spacing 
            return c::cs }
    
    
    ///Primary <- Identifier !LEFTARROW / OPEN Expression CLOSE / Literal / Class / DOT
    //Had to name this method pPrimary to avoid conflict with Primary discriminated union
    let rec pPrimary =
        parser {
            let! id = Identifier
            do! !~ LEFTARROW
            return Primary.Identifier(id) }
        +++ 
        parser {
            do! forget OPEN 
            let! exp = Expression
            do! forget CLOSE 
            return Primary.Expression(exp) }        
        +++ 
        parser {
            let! lit = Literal
            return Primary.Literal(lit) }        
        +++ 
        parser {
            let! cls = Class
            return Primary.Class(cls) }
        +++ 
        (DOT >>! Dot)
        
    
    ///SequenceItem <- (AND / NOT)? Primary (QUESTION / STAR / PLUS)?
    and SequenceItem =
        let prefix = (AND >>! Prefix.And) +++ (NOT >>! Prefix.Not)
        let suffix = (QUESTION >>! Suffix.Question) +++ (STAR >>! Suffix.Star) +++ (PLUS >>! Suffix.Plus)
        parser {
            let! pre = !? prefix
            let! pri = pPrimary
            let! suf = !? suffix
            return {primaryItem=pri;itemPrefix=pre;itemSuffix=suf} }
    
    ///Sequence <- SequenceItem*
    and Sequence = repeat SequenceItem 
        
    ///Expression <- Sequence (SLASH Sequence)*
    and Expression =
        parser {
            let! s = Sequence
            let! sl = repeat (SLASH >>. Sequence)
            return s::sl }
        
    ///Definition <- Identifier LEFTARROW Expression
    let Definition =
        parser {
            let! id = Identifier
            do! forget LEFTARROW 
            let! ex = Expression
            return {name=id;exp=ex} }
            
    ///Grammar    <- Spacing Definition+ EndOfFile
    let Grammar = Spacing >>. repeat1 Definition .>> EndOfFile