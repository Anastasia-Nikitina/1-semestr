module Program

open Argu 
open System
open Functions

type CLIArguments =
    |First of x:int 
    |Second of x:int
    |Third of  x:int * n:int
    |Fourth of right:int * left:int * n:int
    |Fifth
    |Sixth of i: int * j:int * n:int 
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | First _ -> "Задача номер 1"
            | Second _ -> "Задача номер 2"
            | Third _ -> "Задача номер 3"
            | Fourth _ -> "Задача номер 4"
            | Fifth _ -> "Задача номер 5"
            | Sixth _ -> "Задача номер 6"
[<EntryPoint>]
  let main (argv: string array) =
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework_2")
        let results = parser.Parse(argv)
        let args = parser.ParseCommandLine argv

        if results.Contains (First)
        then
            let x = args.GetResult(First)
            let res=Functions.first x
            printfn "First result = %A" res
        elif results.Contains(Second)
        then
            let x=args.GetResult(Second)
            let res=Functions.second x
            printfn "Second result = %A" res
        elif results.Contains(Third)
        then
            let n, x = args.GetResult(Third) 
            let arr=Functions.rndArray n
            printf "Generated array:  " 
            printfn "%A" arr
            let res = Functions.third (arr, x)
            printf "Third result = %A" res
        elif results.Contains(Fourth)
        then
            let right, left, n = args.GetResult(Fourth)
            let arr=Functions.rndArray n
            printf "Generated array: ' " 
            printfn "%A" arr
            let res = Functions.fourth(arr, left, right)
            printf "Fourth result = %A" res
        elif results.Contains(Fifth)
        then
            let arr = Functions.rndArray 2
            printf "Generated array: "
            printfn "%A" arr
            let res = Functions.fifth(arr)
            printf "Fifth result = %A" res
        elif results.Contains(Sixth)
        then
            let i, j, n = args.GetResult(Sixth)
            let arr = Functions.rndArray n
            printf "Generated array: "
            printfn "%A" arr
            let res = Functions.sixth(arr, i, j)
            printfn "sixth result = %A" res
        else printfn "There is no task with this number"
        0
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1
