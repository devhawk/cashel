#light

(*
ometa OMetaParser <: Parser {
  FromTo :x :y   = Seq((x)) (~Seq((y)) Character)* Seq((y)),
  Space          = Super((Space)) | FromTo(("//"), ("\n")) | FromTo(("/*"), ("*/")),
  NameFirst      = '_' | '$' | Letter,
  NameRest       = NameFirst | Digit,
  TSName         = FirstAndRest(("NameFirst"), ("NameRest")):xs              -> { xs.As<string>() },
  Name           = Spaces TSName,
  EscapedChar    = '\\' Character:c                                         -> { System.Text.RegularExpressions.Regex.Unescape("\\" + c.As<string>())[0] }
                 | Character,
  TSString       = '\'' (~'\'' EscapedChar)*:xs '\''                         -> { xs.As<string>() },
  Characters     = '`' '`' (~('\'' '\'') EscapedChar)*:xs '\'' '\''          -> { Sugar.Cons("App", "Seq", xs.ToProgramString()) },
  SCharacters    = '"'     (~'"'    EscapedChar)*:xs '"'                -> { Sugar.Cons("App", "Token",  xs.ToProgramString() ) },
  String         = (('#' | '`') TSName | TSString):xs                  -> { Sugar.Cons("App", "Exactly", xs.ToProgramString() ) },
  Number         = ('-' | Empty -> { "" }):sign Digit+:ds -> { Sugar.Cons("App", "Exactly", (sign != OMetaList<HostExpression>.Nil ? sign.As<string>() : "") + ds.As<string>()) },
  Keyword :xs    = Token((xs)) ~LetterOrDigit                            -> { xs },
  HostExpr       = Foreign((typeof(OMetaSharp.UnitTests.ManuallyCreatedCode.ManualCSharpRecognizer)), ("ParenExpr")),
  AtomicHostExpr = Foreign((typeof(OMetaSharp.UnitTests.ManuallyCreatedCode.ManualCSharpRecognizer)), ("Block")),
  Args           = "(" ListOf(("HostExpr"), (",")):xs ")"                   -> { xs }
                 | Empty                                               -> { OMetaList<HostExpression>.Nil },
  Application    = Name:rule Args:ags                                  -> { Sugar.Cons("App", rule, ags) },
  SemAction      = ("!" | "->") AtomicHostExpr:x                       -> { Sugar.Cons("Act",  x) },
  SemPred        = "?" HostExpr:x                       -> { Sugar.Cons("Pred", x) },
  Expr           = ListOf(("Expr4"), ("|")):xs                         -> { Sugar.HackedInnerConcat("Or", xs) },
  Expr4          = Expr3*:xs                                           -> { Sugar.HackedInnerConcat("And", xs) },
  OptIter :x     = "*"                                                 -> { Sugar.Cons("Many",  x) }
                 | "+"                                                 -> { Sugar.Cons("Many1", x) }
                 | Empty                                               -> { x },
  Expr3          = Expr2:x OptIter((x)):x ( ':' Name:n                   -> { Sugar.StatementCons(
																			  () => Get<VariableSet>("Locals").Add(n.ToString())
																				   ,
																				   Sugar.Cons("Set", n, x)) }
                                          | Empty                        -> { x }
                                          )
                 | ":" Name:n                                          -> { Sugar.StatementCons(
																			() => Get<VariableSet>("Locals").Add(n.ToString()),
																		    Sugar.Cons("Set", n, Sugar.Cons("App", "Anything"))) },                 
  Expr2          = "~" Expr2:x                                         -> { Sugar.Cons("Not", x) }
                 | "&" Expr1:x                                         -> { Sugar.Cons("Lookahead", x) }
                 | Expr1,
  Expr1          = Application | SemAction | SemPred
                 | ( Keyword(("undefined")) | Keyword(("nil"))
                 |   Keyword(("true"))      | Keyword(("false")) ):x       -> { Sugar.Cons("App", "Exactly", x) }
                 | Spaces (Characters | SCharacters | String | Number)
                 | "[" Expr:x "]"                                      -> { Sugar.Cons("Form", x) }
                 | "(" Expr:x ")"                                      -> { x },
  RuleName       = Name
                 | Spaces TSString,
  Rule           = &(RuleName:n) !{Set("Locals", new VariableSet())}
					RulePart((n)):x ("," RulePart((n)))*:xs           -> { Sugar.Cons("Rule", n, Get<VariableSet>("Locals"), Sugar.Cons("Or", x, xs)) },
  RulePart :rn   = RuleName:n ?(n.Equals(rn)) Expr4:b1 ( "=" Expr:b2        -> { Sugar.Cons("And", b1, b2) }
                                                  | Empty              -> { b1 }
                                                  ),
  Grammar        = Keyword(("ometa")) Name:n
                     ( "<:" Name | Empty -> { "OMeta" } ):sn
                     "{" ListOf(("Rule"), (",")):rs "}"                -> { Sugar.ConsWithFlatten("Grammar", n, sn, rs) }
} 
*)