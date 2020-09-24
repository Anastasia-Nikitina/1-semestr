module Main

open Argu
open System
open Functions

type CLIArguments =
    |First of x:int 
    |Second of x:int
    |Third
    |Fourth
    |Fifth
    |Sixth 
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | First _ -> "Задача номер 1"
            | Second _ -> "Задача номер 2"
            | Third -> "Задача номер 3"
            | Fourth -> "Задача номер 4"
            | Fifth -> "Задача номер 5"
            | Sixth -> "Задача номер 6"

[<EntryPoint>]
let main (argv: string array) =
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "hw_2")
        let results = parser.Parse(argv)
        let args = parser.ParseCommandLine argv

        if args.Contains(First)
        then
            let t1Params = args.GetResult(First)
            printfn "Task1"
        elif args.Contains(Second)
        then printfn "Task2"
        elif args.Contains(Sixth)
        then
            //let i, j, n = args.GetResult(Sixth)
            //let res = six i j n
            let res = sixth
            printfn "Task6 result = %A" res
        else printfn "Other"
        0
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1

