module Functions
open System

let first x = x*x*x*x+x*x*x+x*x+x+1

let second x=
    let y=x*x
    let z=y+x
    y*z+z+1

let third third=
    printfn "Введите количество элементов в массиве:"
    let n=System.Int32.Parse(Console.ReadLine())
    printfn "Элементы не большие, чем:"
    let c = System.Int32.Parse(Console.ReadLine())
    let rnd=System.Random()
    let arr=[|for i in 0..n->rnd.Next()|]
    for i = 0 to n do
        if arr.[i]<=c then printfn "%d" i

let fourth fourth=
    printfn "Введите диапазон"
    printfn "Левая граница:"
    let left=System.Int32.Parse(Console.ReadLine())
    printfn "Правая граница:"
    let right=System.Int32.Parse(Console.ReadLine())
    printfn"Введите число элементов в массиве:"
    let n=System.Int32.Parse(Console.ReadLine())
    let rnd=System.Random()
    let arr=[|for i in 0..n->rnd.Next()|]
    for i=0 to n do
        if (arr.[i]<left) && (arr.[i]>right) then printfn "%d" i
let fifth fifth=
    let rnd=System.Random()
    let arr=[|for i in 0..1->rnd.Next()|]
    arr.[0]=arr.[1]*arr.[0]
    arr.[1]=arr.[0]/arr.[1]
    arr.[0]=arr.[0]/arr.[1]
let sixth sixth= // hint: remove unused variable
    printfn"Введите число элементов в массиве:"
    let n=System.Int32.Parse(Console.ReadLine())
    let rnd=System.Random()
    let arr=[|for i in 0..n->rnd.Next()|]
    printfn"Введите индекс первого элемента:"
    let i=System.Int32.Parse(Console.ReadLine())
    printfn"Введите индекс второго элемента:"
    let j=System.Int32.Parse(Console.ReadLine())
    arr.[i]=arr.[j]*arr.[i]
    arr.[j]=arr.[i]/arr.[j]
    arr.[i]=arr.[i]/arr.[j]
