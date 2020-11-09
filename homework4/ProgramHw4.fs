module ProgramHw4

open Argu 
open System
open functionsHw4
open System
open System.IO

type CLIArguments =
    |BubbleSortArray 
    |BubbleSortList 
    |QuickSortList
    |QuickSortArray
    |Packing32to64
    |Unpacking64to32
    |Packing16to64
    |Unpacking64to16
    interface IArgParserTemplate with
       member s.Usage =
          match s with
           | BubbleSortArray _ -> "Sorting array by bubble-sort"
           | BubbleSortList  _ -> "Sorting list by bubble-sort"
           | QuickSortList _ -> "Sorting list by quick sort"
           | QuickSortArray _ -> "Sorting array by quick sort"
           | Packing32to64 _ -> "Packing 32-bit in 64-bit numbers"
           | Unpacking64to32 _ -> "Unpacking 64-bit in 32-bit numbers"
           | Packing16to64 _ -> "Packing 16-bit in 64-bit numbers"
           | Unpacking64to16 _ -> "Unpacking 64-bit in 16-bit numbers"
[<EntryPoint>]
 let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "homework4") 
    let results = parser.Parse argv

    let mainFun fun1 fun2 fun3 =
        printfn "Enter the name of the input file:"
        let res = fun1 (fun2(Console.ReadLine()))
        printfn "Enter the name of the output file:"
        fun3(Console.ReadLine()) res
    if   results.Contains BubbleSortArray then mainFun (bubbleSortOfArray) (readArray) (writeArray)
    elif results.Contains BubbleSortList then mainFun (bubbleSortOfList) (readList) (writeList)
    elif results.Contains QuickSortList then mainFun (quickSortOfList) (readList) (writeList)   
    elif results.Contains QuickSortArray then mainFun (quickSortOfArray) (readArray) (writeArray)
    elif results.Contains Packing32to64
    then
        printfn "Enter two numbers:"
        let a = Console.ReadLine() |> int32
        let b = Console.ReadLine() |> int32
        printfn "Packed numbers: %A" (packing32BitNumber (a, b))
    elif results.Contains Unpacking64to32
    then
        printfn "Enter number:"
        let x = Console.ReadLine() |> int64
        printfn "Unpacked numbers: %A" (unPacking64to32BitNumber x)
    elif results.Contains Packing16to64
    then
        printfn "Enter four numbers:"
        let a = Console.ReadLine() |> int16
        let b = Console.ReadLine() |> int16
        let c = Console.ReadLine() |> int16
        let d = Console.ReadLine() |> int16
        printfn "Packed numbers: %A" (packing16BitNumber (a, b, c, d))
    elif results.Contains Unpacking64to16
    then
        printfn "Enter number:"
        let x = Console.ReadLine() |> int64
        printfn "Unpacked numbers: %A" (unPacking64to16BitNumber x)
    else
        parser.PrintUsage() |> printfn "%s"
    0
       










 
