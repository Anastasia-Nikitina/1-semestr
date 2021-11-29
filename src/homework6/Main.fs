module Main
open hw6
open System

[<EntryPoint>]
let main(argv: array<string>) =
    printfn "Enter the path of first input file"
    let matrix1 = readBoolMatrix (Console.ReadLine())
    printfn "Enter the path of second input file"
    let matrix2 = readBoolMatrix (Console.ReadLine())
    printfn "Enter the path of output file"
    writeArray2D (Console.ReadLine()) (BoolMatrixToArray (multBoolMatrix (matrix1, matrix2)))
    0
