module Parser
open FParsec
open NUnit.Framework
open FsUnit
open LispTypes

type LispState = unit
type Parser<'t> = Parser<'t, LispState>

let pSymbol: Parser<_> = anyOf "!#$%&|*+-/:<=>?@^_~"
let notQuoteChar: Parser<_> = noneOf (Seq.ofArray [| '\"' |])
let unquotedString = manyChars notQuoteChar
let parseString: Parser<LispVal> =
    unquotedString
    |> between (pstring "\"") (pstring "\"")
    |>> LispString
let parseAtom =
    pipe2
        (letter <|> pSymbol)
        (manyChars (letter <|> digit <|> pSymbol))
        (fun s rest ->
            let atom = $"%c{s}%s{rest}"
            match atom with
            | "#t" -> LispBool true
            | "#f" -> LispBool false
            | _ -> LispAtom atom
        )
let parseNumber: Parser<_> =
    pint64 |>> LispNumber
let parseExpr =
    parseAtom <|> parseString <|> parseNumber

let readExpr input =
    match run (spaces >>. parseExpr) input with
    | Failure (_, err, _) -> $"No match: {err.ToString()}"
    | Success _ -> "Found value"

let checkResult v r = match r with
    | ParserResult.Success(e, _, _) -> should equal v e
    | _ -> Assert.Fail "parse Failed"
let checkParseFailed r = match r with
    | ParserResult.Success _ -> Assert.Fail("Expect parse fail")
    | _ -> ()
[<Test>]
let ``parse atom test`` () =
    run parseAtom "#t" |> checkResult (LispBool true)
    run parseAtom "#f" |> checkResult (LispBool false)
    run parseAtom "#test" |> checkResult (LispAtom "#test")
    run parseAtom "test" |> checkResult (LispAtom "test")
    run parseAtom "+" |> checkResult (LispAtom "+")
    run parseAtom "1" |> checkParseFailed