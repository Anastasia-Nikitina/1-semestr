open System
open generator

[<EntryPoint>]
let main argv =
    printfn "Enter the path to file:"
    let path = Console.ReadLine()
    printfn "Enter the size of matrix:"
    let size = Console.ReadLine()
    printfn "Enter the quantity of matrices:"
    let quant = Console.ReadLine()
    printfn "Enter the sparcity of matrix:"
    let sparce = Console.ReadLine()
    printfn "Enter the type of matrix:"
    let typ = Console.ReadLine()

    if typ = "int"
    then genSparseMatrix<int32> path (int size) (int quant) (float sparce)
    if typ = "bool"
    then genSparseMatrix<bool> path (int size) (int quant) (float sparce)
    if typ = "float"
    then genSparseMatrix<float> path (int size) (int quant) (float sparce)
    
    0
