module FunctionsHw3
open System

  
let rec fib1 n =
    if n=0 || n=1
    then n
    elif n > 1
    then fib1(n-1) + fib1(n-2)
    else failwithf "n must be non-negative"

let fib2 n =
    if n = 0 || n = 1
    then n
    elif n > 1 
    then
      let mutable i = 2
      let mutable f0 = 0
      let mutable f1 = 1
      let mutable fsum = f0 + f1
      while i <= n do
        fsum <- f0 + f1
        f0 <- f1
        f1 <- fsum
        i <- i + 1
      fsum
    else failwithf "n must be non-negative"


let fib3 n =
    let rec _go n a b =
        if n = 0
        then a
        else _go (n-1) b (a+b) 
    if n = 0 || n = 1
    then n
    elif n > 1
    then _go n 0 1
    else failwithf "n must be non-negative"


let multMatrix (a: int [,]) (b: int [,]) =
    let res = Array2D. zeroCreate (a.GetLength 0) (b.GetLength 1)
    for i = 0 to (a.GetLength 0 - 1) do
      for j = 0 to (b.GetLength 1 - 1) do
       for l = 0 to (a.GetLength 1 - 1) do
          res.[i, j] <- res.[i, j] + a.[i, l]*b.[l, j]
    res
    
let identityMatrix = array2D [|[|1; 0|]; [|0; 1|]|]

let rec powMatrix m n =
    if n = 0
    then identityMatrix
    else multMatrix m (powMatrix m (n-1))

let fib4 n =
    if n >= 0
    then
        let arr = array2D [|[|0;1|]; [|1;1|]|]
        let res = powMatrix arr n
        res.[0, 1] 
    else failwithf "n must be non-negative"

let fib5 n =
    if n >= 0
    then
        let arr = array2D [|[|0; 1|]; [|1; 1|]|]
        let rec _go arr n =
          if n = 0 then identityMatrix
          elif n % 2 = 0
          then multMatrix (_go arr (n/2))(_go arr (n/2))
          else multMatrix (_go arr (n-1)) arr
        let res = _go arr n
        res.[0, 1]
    else failwith "n must be non-negative"   


let fib6 n =
    let arr: int array = Array.zeroCreate (n + 1)
    if n = 0 || n = 1
    then
        for i = 0 to n do
            arr.[n] <- n
    elif n>1
    then
         arr.[0] <- 0
         arr.[1] <- 1
         for i = 2 to n do
           arr.[i] <- arr.[i-1] + arr.[i-2]
    else failwith "n must be non-negative"
    arr




