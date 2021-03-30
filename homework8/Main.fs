open System
open matrices

[<EntryPoint>]
let main(argv: array<string>) =
    let alg = new SemiRing<int> (0, (+), (*))
    let a = readAndExtension(Console.ReadLine())
    let b = readAndExtension(Console.ReadLine())
    let res = mult a b alg
    printfn "%A" res.qt
    0
