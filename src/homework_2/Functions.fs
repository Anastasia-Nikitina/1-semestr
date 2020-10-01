module Functions
open System

let rndArray n =
    let rnd = System.Random()
    Array.init n (fun _ -> rnd.Next(50))

let first x = x*x*x*x + x*x*x + x*x + x + 1

let second x =
    let y = x*x
    let z = y + x
    y*z + z + 1

let third (arr:array<int>, x:int) =
    let mutable j = 0
    for i = 0 to arr.Length - 1 do
        if arr.[i] < x then j <- j + 1
    let result = Array.zeroCreate j
    j <- 0
    for i = 0 to arr.Length - 1 do
        if arr.[i] <= x then
            result.[j] <- i
            j <- j + 1
    result

let fourth (arr:array<int>, left, right ) =
    let mutable j = 0
    for i = 0 to arr.Length-1 do
         if arr.[i] < left || arr.[i] > right then j <- j + 1
    let result = Array.zeroCreate j
    j <- 0
    for i = 0 to arr.Length-1 do
        if arr.[i] < left || arr.[i] > right then
            result.[j] <- i
            j <- j + 1
    result

let fifth (arr: array<int>) =
    arr.[0] <- arr.[1] + arr.[0]
    arr.[1] <- arr.[0] - arr.[1]
    arr.[0] <- arr.[0] - arr.[1]
    arr

let sixth (arr: array<int>, i:int, j:int) =
    if i < arr.Length && j < arr.Length
    then
      arr.[i] <- arr.[j] + arr.[i]
      arr.[j] <- arr.[i] - arr.[j]
      arr.[i] <- arr.[i] - arr.[j]
      arr
    else
      printfn "Введенные индексы некорректны"   
      arr
