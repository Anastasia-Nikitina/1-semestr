open System
open code
open Argu
type CLIArguments =
    | MultAllMatrices of inDir: string 
    | MultSomeMatrices of inDir: string * count: int
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | MultAllMatrices _ -> "Multiplication of all pairs of matrices"
            | MultSomeMatrices _ -> "Multiplication of a given number pairs of matrices"

[<EntryPoint>]
let main (argv: string array) =
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "MatricesMailbox")
        let results = parser.Parse(argv)
        let args = parser.ParseCommandLine argv
        if args.Contains(MultAllMatrices)
        then
            let inDir = args.GetResult(MultAllMatrices)
            processAllFilesAsync fixConst inDir
        elif args.Contains(MultSomeMatrices)
        then
            let inDir, count = args.GetResult(MultSomeMatrices)
            processSomeFilesAsync fixConst inDir count 
        else printfn "Input is incorrected"
        0
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1
