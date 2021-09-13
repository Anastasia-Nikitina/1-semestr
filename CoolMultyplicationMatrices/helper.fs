module helper
open FunctionsHw3
open matrices

let multMatrix (a: int [,]) (b: int [,]) =
    if a.GetLength 0 = b.GetLength 1
    then
      let res = Array2D.zeroCreate (a.GetLength 0) (b.GetLength 1)
      for i = 0 to (a.GetLength 0 - 1) do
        for j = 0 to (b.GetLength 1 - 1) do
          for l = 0 to (a.GetLength 1 - 1) do
            res.[i, j] <- res.[i, j] + a.[i, l]*b.[l, j]
      res
    else failwith "Size of matrices is not correct"

let multMatrixPar (m1: int[,]) (m2: int[,]) =
    if (m1.GetLength 1 = m2.GetLength 0) then
        let a = m1.GetLength 0
        let b = m1.GetLength 1
        let c = m2.GetLength 1
        let res = Array2D.zeroCreate a c
        [ for i in 0 .. a - 1 ->
            async {
                do
                    for j in 0 .. c - 1 do
                        for k in 0 .. b - 1 do
                            res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
            }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        res
    else failwith "It's impossible to multiply matrixes of this sizes"

let calcOfSparsity (m1: int[,]) (m2: int[,]) =
    let go (m: int[,]) =
        let mutable k = 0
        for i = 0 to m.GetLength(0) - 1 do
            for j = 0 to m.GetLength(1) - 1 do
                if m.[i, j] <> 0
                then k <- k + 1
        k
    (float (go m1 / (m1.GetLength(0)*m1.GetLength(1))), float (go m2 / (m2.GetLength(0)*m2.GetLength(1))))
