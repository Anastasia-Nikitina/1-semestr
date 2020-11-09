module Program

open Argu 
open System
open Functions
type CLIArguments =
    |FirstHw2 of x:int 
    |SecondHw2 of x:int
    |ThirdHw2 of  x:int * n:int
    |FourthHw2 of right:int * left:int * n:int
    |FifthHw2
    |SixthHw2 of i:int * j:int * n:int
    |FirstHw3 of x:int
    |SecondHw3 of x:int
    |ThirdHw3 of x:int
    |FourthHw3 of x:int 
    |FifthHw3 of x:int
    |SixthHw3 of x:int
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | FirstHw2 _ -> "Task 1, hw2"
            | SecondHw2 _ -> "Task 2, hw2"
            | ThirdHw2 _ -> "Task 3, hw2"
            | FourthHw2 _ -> "Task 4, hw2"
            | FifthHw2 _ -> "Task 5, hw2"
            | SixthHw2 _ -> "Task 6, hw2"
            | FirstHw3 _ -> "Task 1, hw3"
            | SecondHw3 _ -> "Task 2, hw3"
            | ThirdHw3 _ -> "Task 3, hw3"
            | FourthHw3 _ -> "Task 4, hw3"
            | FifthHw3 _ -> "Task 5, hw3"
            | SixthHw3 _ -> "Task 6, hw3"
[<EntryPoint>]
  let main (argv: string array) =
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "homework_2")
        let results = parser.Parse(argv)
        let args = parser.ParseCommandLine argv

        //homework2
        if results.Contains(FirstHw2)
        then
            let x = args.GetResult(FirstHw2)
            let res = Functions.first x
            printfn "First result of hw 2 = %A" res
        elif results.Contains(SecondHw2)
        then
            let x = args.GetResult(SecondHw2)
            let res = Functions.second x
            printfn "Second result of hw 2 = %A" res
        elif results.Contains(ThirdHw2)
        then
            let n, x = args.GetResult(ThirdHw2) 
            let arr = Functions.rndArray n
            printf "Generated array:  " 
            printfn "%A" arr
            let res = Functions.third(arr, x)
            printf "Third result of hw 2 = %A" res
        elif results.Contains(FourthHw2)
        then
            let right, left, n = args.GetResult(FourthHw2)
            let arr = Functions.rndArray n
            printf "Generated array: ' " 
            printfn "%A" arr
            let res = Functions.fourth(arr, left, right)
            printf "Fourth result of hw 2 = %A" res
        elif results.Contains(FifthHw2)
        then
            let arr = Functions.rndArray 2
            printf "Generated array: "
            printfn "%A" arr
            let res = Functions.fifth(arr)
            printf "Fifth result of hw 2 = %A" res
        elif results.Contains(SixthHw2)
        then
            let i, j, n = args.GetResult(SixthHw2)
            let arr = Functions.rndArray n
            printf "Generated array: "
            printfn "%A" arr
            let res = Functions.sixth(arr, i, j)
            printfn "sixth result of hw 2 = %A" res
            //homework3
        elif results.Contains(FirstHw3)
        then
            let x = args.GetResult(FirstHw3)
            let res = FunctionsHw3.fib1 x
            printfn "First result of hw 3 = %A" res
        elif results.Contains(SecondHw3)
        then
            let x = args.GetResult(SecondHw3)
            let res = FunctionsHw3.fib2 x
            printfn "Second result of hw 3 = %A" res
        elif results.Contains(ThirdHw3)
        then
            let x = args.GetResult(ThirdHw3)
            let res = FunctionsHw3.fib3 x
            printfn "Third result of hw 3 = %A" res
        elif results.Contains(FourthHw3)
        then
            let x = args.GetResult(FourthHw3)
            let res = FunctionsHw3.fib4 x
            printfn "Fourth result of hw 3 = %A" res
        elif results.Contains(FifthHw3)
        then
            let x = args.GetResult(FifthHw3)
            let res = FunctionsHw3.fib5 x
            printfn "Fifth result of hw 3 = %A" res
        elif results.Contains(SixthHw3)
        then
            let x = args.GetResult(SixthHw3)
            let res = FunctionsHw3.fib6 x
            printfn "Sixth result of hw 3 = %A" res
        else printfn "There is no task with this number"
        0
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1
