open System
open System.IO
open FSharp.Text.Lexing
open DslType

[<EntryPoint>]
let main argv =
    let description, sourceText =
        if argv.Length = 0 then
            "stdin", stdin.ReadToEnd()
        else
            let path = argv[0]
            if not (File.Exists path) then
                eprintfn "Input file '%s' does not exist." path
                exit 1
            path, File.ReadAllText path

    let lexbuf = LexBuffer<char>.FromString sourceText

    try
        let ast = TypeParser.main TypeLexer.token lexbuf
        printfn "%s" (Pretty.program ast)
        0
    with ex ->
        let pos = lexbuf.EndPos
        eprintfn "Parsing failed in %s at line %d, column %d: %s" description (pos.Line + 1) (pos.Column + 1) ex.Message
        1
