open Parser

[<EntryPoint>]
let main argv =
    let input = if argv.Length <> 0 then argv[0] else ""
    input
    |> readExpr
    |> printfn "%s\n"
    0
