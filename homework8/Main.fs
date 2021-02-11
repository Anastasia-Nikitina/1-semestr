open System
open long_arithmetic
open functionsHw4
open MyList

[<EntryPoint>]
let main(argv: array<string>) =
    printfn "Enter the first number"
    let number1 = TransformStringToMyList (Console.ReadLine())   
    printfn "Enter the second number"
    let number2 = TransformStringToMyList (Console.ReadLine())   
    printfn "%A" (addition (number1, number2)) 
    (*printfn "Enter the first number"
    let number1 = TransformStringToMyList (Console.ReadLine())
    printfn "%A" number1
    printfn "Enter the second number"
    let number2 = TransformStringToMyList1 (Console.ReadLine())
    printfn "%A" number2
    printfn "%A" (substraction (number1, number2))*)
    0
    
    
