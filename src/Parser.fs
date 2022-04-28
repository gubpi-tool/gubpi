(****************************************************************************************)
(*                                                                                      *)
(*                                      Parser.fs                                       *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* A Parser For SPCF                                                                    *)
(*                                                                                      *)
(****************************************************************************************)
module Parser

open FParsec

open Expression
open PrimitiveFunctions
open PrimitiveDistributions
open Util

// All keywords that may not be variable names
let keywords =
    [ "if"
      "then"
      "else"
      "fix"
      "sample"
      "score"
      "let"
      "letrec"
      "in"
      "\\"
      "λ"
      "and" ]
    @ List.map (fun (x: PrimitiveFunction) -> x.Name) primFunctions
      @ List.map (fun (x: PrimitiveDistribution) -> x.Name) distributions

let multilineComment =
    skipString "(*"
    .>> charsTillString "*)" true System.Int32.MaxValue

let comment =
    (skipChar '#' .>> restOfLine false)
    <|> multilineComment

let ws = spaces .>> sepEndBy comment spaces // whitespace and comment parser

let BP (p: Parser<_, _>) stream = p stream // set a breakpoint here


// ================================= EXPR Parser =================================

// Create the recursive Parser Parser for SPCF expressions
let expParser, expParserRef = createParserForwardedToRef ()

let parParser =
    between (skipChar '(') (skipChar ')') expParser

let identStart = letter <|> pchar '_'

let identCont =
    letter <|> digit <|> pchar '\'' <|> pchar '_'

// Don't just use pstring for keywords, otherwise `iffy` will parse as `if fy`
let skipKeyword string =
    skipString string .>>? notFollowedBy identCont

let identParser =
    identStart .>>. manyChars identCont
    >>= (fun (a, b) ->
        let x = string a + b

        if List.contains x keywords then
            fail $"Variable name `{x}` cannot be keyword"
        else
            preturn x)
    |> attempt

let varParser = identParser |>> Variable

let numeralParser =
    // pfloat uses NumberLiteralOptions.DefaultFloat, which includes "nan", "inf", and + or - at the start.
    // This confuses the rest of the parser, so we only parse the pure digits, decimal point, and exponent here
    let numberFormat =
        NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent

    numberLiteral numberFormat "number"
    |>> fun nl ->
            System.Double.Parse(nl.String, System.Globalization.CultureInfo.InvariantCulture)
            |> Numeral

let listLiteralParser =
    let listContent =
        sepEndBy expParser (skipChar ',')
        .>>. opt (skipChar '|' >>. expParser)

    let transformList (elements, rest) =
        List.fold (fun acc el -> ConsList(el, acc)) (Option.defaultValue (EmptyList None) rest) elements

    between (skipChar '[' >>. ws) (skipChar ']') (listContent |>> transformList)

let listMatchParser =
    // Parses a match on lists (FoldList)
    let emptyCase =
        skipChar '|'
        >>. ws
        >>. skipChar '['
        >>. ws
        >>. skipChar ']'
        >>. ws
        >>. skipString "->"

    let consPattern =
        skipChar '[' .>> ws >>. identParser
        .>> ws
        .>> skipChar '|'
        .>> ws
        .>>. identParser
        .>> ws
        .>> skipChar ']'

    let consCase =
        skipChar '|' >>. ws >>. consPattern
        .>> ws
        .>> skipString "->"

    skipKeyword "match" >>. expParser .>> emptyCase
    .>>. expParser
    .>>. consCase
    .>>. expParser
    |>> fun (((scrutinee, emptyExpr), (x, xs)), consExpr) -> FoldList(scrutinee, emptyExpr, x, xs, consExpr)


let absParser =
    // Parses a lambda abstraction
    let lambda = anyOf "\\λ" .>> ws

    pipe2 (lambda >>. identParser .>> ws) (skipChar '.' >>. expParser) (fun x y -> Abstraction(x, None, y))

let conditionalParser =
    // Parses a conditional
    let compParser =
        choice [ stringReturn "<=" LEQ
                 stringReturn "<" LT
                 stringReturn ">=" GEQ
                 stringReturn ">" GT ]

    let explictIneParser =
        pipe3 (pchar '(' >>. expParser .>> ws) (compParser .>> ws) (expParser .>> ws .>> pchar ')') (fun x c z ->
            PrimF(SUB, [ x; z ]), c)

    let implictIneParser = expParser |>> (fun x -> (x, LEQ))

    let ineqParser =
        ws
        >>. ((attempt explictIneParser) <|> implictIneParser)

    let conjunctionParser =
        sepBy1 (ineqParser .>> ws) (pstring "and" .>> ws)


    let constructConditionalFromComp ((g, c), N, P) =
        match c with
        | LEQ -> Conditional(g, N, P)
        | LT -> Conditional(PrimF(NEG, [ g ]), P, N)
        | GEQ -> Conditional(PrimF(NEG, [ g ]), N, P)
        | GT -> Conditional(g, P, N)

    let rec constructConjunctiveConditional guards N P =
        match guards with
        | [] -> failwith "No support for empty conjunctions"
        | [ x ] -> constructConditionalFromComp (x, N, P)
        | x :: xs -> constructConditionalFromComp (x, constructConjunctiveConditional xs N P, P)

    pipe3
        (skipKeyword "if" >>. conjunctionParser)
        (skipKeyword "then" >>. expParser)
        (skipKeyword "else" >>. expParser)
        constructConjunctiveConditional

let scoreParser =
    // parses a score expression
    (skipKeyword "score" >>. ws >>. parParser)
    |>> Score

let fixpointParser =
    // parses a fixpoint expression
    pipe2
        (skipKeyword "fix" >>. ws >>. identParser .>> ws
         .>>. identParser
         .>> ws)
        (skipChar '.' >>. expParser)
        (fun (a, b) M -> Fixpoint(a, b, None, M))

let letAndTupleMatchParser =
    // Parses a let expression.
    // An expression let x = N in M is a let expression and gets translated to (\x. M)N
    // An expression let x, y = N in M is a match on a tuple (and gets translated to a TupleMatch)
    let identListParser =
        sepBy1 (identParser .>> ws) (skipChar ',' .>> ws)

    pipe3
        (skipKeyword "let" >>. ws >>. identListParser
         .>> ws)
        (skipChar '=' >>. expParser)
        (skipKeyword "in" >>. expParser)
        (fun x N M ->
            match x with
            | [ v ] -> Application(Abstraction(v, None, M), N)
            | _ -> TupleMatch(x, N, M))

let letrecParser =
    // Parses a letrec, a simpler construction to define fixpoints
    pipe3
        (skipKeyword "letrec" >>. ws >>. identParser .>> ws
         .>>. identParser
         .>> ws)
        (skipChar '=' >>. expParser)
        (skipKeyword "in" >>. expParser)
        (fun (f, x) N M -> Application(Abstraction(f, None, M), Fixpoint(f, x, None, N)))

let argumentListParser =
    sepEndBy expParser (skipChar ',')
    |> between (skipChar '(') (skipChar ')')

let sampleParser =
    let distributionSymbolParser =
        choice [ for d in distributions do
                     stringReturn d.Name d .>>? notFollowedBy identCont ]

    skipKeyword "sample"
    >>. ws
    >>. distributionSymbolParser
    .>>. argumentListParser
    >>= (fun (d, y) ->
        if d.Parameters = y.Length then
            preturn (Sample(d.Symbol, y))
        else
            failFatally $"Wrong number of arguments for `{d}`")

let primitiveFunctionParser =
    // Parse a primitive function by checking the list of available primitive functions
    let primFunctionSymbolParser =
        choice [ for f in primFunctions do
                     stringReturn f.Name f .>>? notFollowedBy identCont ]

    (attempt primFunctionSymbolParser)
    .>>. argumentListParser
    >>= (fun (f, y) ->
        if f.Arity = y.Length then
            preturn (PrimF(f.Symbol, y))
        else
            failFatally $"Wrong number of arguments for `{f}`")

let tupleParser =
    // Parses a tuple constant, for example (| 1, 2  |)
    between (skipString "(|" .>> ws) (skipString "|)") (sepEndBy expParser (skipChar ','))
    >>= (fun x ->
        if List.length x > 1 then
            preturn (Tuple x)
        else
            failFatally "A tuple must have at least two-elements")

// Parse terms that don't require parentheses when used as an argument of an application
let termParser =
    ws
    >>. choice [ tupleParser
                 parParser
                 scoreParser
                 primitiveFunctionParser
                 numeralParser
                 listLiteralParser
                 sampleParser
                 varParser ]
    .>> ws

let appParser =
    many1 termParser
    |>> List.reduce (fun x y -> Application(x, y))

let operatorExprParser =
    new OperatorPrecedenceParser<Expr, unit, unit>()

operatorExprParser.TermParser <- appParser

// a helper function for adding infix operators to opp
let addInfixOperator string precedence associativity funSym =
    operatorExprParser.AddOperator(
        InfixOperator(string, ws, precedence, associativity, (fun e1 e2 -> PrimF(funSym, [ e1; e2 ])))
    )

let addPrefixOperator string precedence associativity funSym =
    operatorExprParser.AddOperator(
        PrefixOperator(string, ws, precedence, associativity, (fun e -> PrimF(funSym, [ e ])))
    )

let additionPrecedence = 10
let multiplicationPrecedence = 20

// the operator definitions:
operatorExprParser.AddOperator(PrefixOperator("+", ws, multiplicationPrecedence, false, id))

operatorExprParser.AddOperator(
    PrefixOperator(
        "-",
        ws,
        multiplicationPrecedence,
        false,
        function
        | Numeral n -> Numeral(-n)
        | e -> PrimF(NEG, [ e ])
    )
)
// Add all infix operator to the parser
addInfixOperator "+" additionPrecedence Associativity.Left ADD
addInfixOperator "-" additionPrecedence Associativity.Left SUB
addInfixOperator "*" multiplicationPrecedence Associativity.Left MUL
addInfixOperator "/" multiplicationPrecedence Associativity.Left DIV

// Add Cons as an infix operator
operatorExprParser.AddOperator(InfixOperator("::", ws, 5, Associativity.Right, (fun e1 e2 -> ConsList(e1, e2))))

// Expressions that do require parentheses when used as an argument of an application
let singleExprParser =
    ws
    >>. choice [ conditionalParser
                 listMatchParser
                 absParser
                 fixpointParser
                 letrecParser
                 letAndTupleMatchParser
                 operatorExprParser.ExpressionParser ]
    .>> ws

do
    expParserRef
    := sepBy1 singleExprParser (skipChar ';')
       |>> List.reduce (fun e1 e2 -> Application(Abstraction("", None, e2), e1))

// Parse the configuration line at the beginning of the input, modifies the existing configuration
let configLineParser (config: Hyperparameters) =
    let methodConfig =
        skipString "method"
        >>. spaces
        >>. ((skipString "boxes" >>% ApproximationMethod.Boxes)
             <|> (skipString "polytopes"
                  >>% ApproximationMethod.Polytopes))
        |>> (fun m -> { config with method = m })

    let depthConfig =
        skipString "depth" >>. spaces >>. pint32
        |>> (fun d -> { config with depth = d })

    let splitsConfig =
        skipString "splits" >>. spaces >>. pint32
        |>> (fun s -> { config with splits = s })

    let outputSplitProgressConfig =
        skipString "outputSplitProgress"
        >>. spaces
        >>. pint32
        |>> (fun d -> { config with outputSplitProgress = if d = 0 then false else true })

    let outputCurrentPathConfig =
        skipString "outputCurrentPath"
        >>. spaces
        >>. pint32
        |>> (fun d -> { config with outputCurrentPath = if d = 0 then false else true })

    let outputCurrentAreaConfig =
        skipString "outputCurrentArea"
        >>. spaces
        >>. pint32
        |>> (fun d -> { config with outputCurrentArea = if d = 0 then false else true })

    let discretizationConfig =
        skipString "discretization" >>. spaces >>. pfloat
        .>> spaces
        .>>. pfloat
        .>> spaces
        .>>. pfloat
        |>> (fun ((a, b), c) -> { config with discretization = { Start = a; End = b; StepSize = c } })

    let epsilonScoreConfig =
        skipString "epsilonScore" >>. spaces >>. pfloat
        |>> (fun d -> { config with epsilonScore = d })

    let espilonVarConfig =
        skipString "epsilonVar" >>. spaces >>. pfloat
        |>> (fun d -> { config with epsilonVar = d })

    skipChar '#'
    >>. spaces
    >>. choice [ methodConfig
                 depthConfig
                 splitsConfig
                 discretizationConfig
                 outputSplitProgressConfig
                 outputCurrentPathConfig
                 outputCurrentAreaConfig
                 epsilonScoreConfig
                 espilonVarConfig ]
    .>> spaces

let rec configParser (config: Hyperparameters) =
    (attempt (configLineParser config) >>= configParser)
    <|>% config


/// Parses a SPCF program with GuBPI configuration
let parseProgram (config: Hyperparameters) (s: string) =
    let full =
        configParser config .>>. expParser .>> eof

    let res = run full s

    match res with
    | Success (res, _, _) -> res
    | Failure (err, _, _) ->
        printfn $"The provided term could not be parsed. Output by FParsec: %s{err}"
        failwith "Parse Error"