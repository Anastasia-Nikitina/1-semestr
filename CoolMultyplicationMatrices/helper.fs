module helper
open FunctionsHw3
open matrices

let calcOfSparsity (m1: int[,]) (m2: int[,]) =
    let go (m: int[,]) =
        let mutable k = 0
        for i = 0 to m.GetLength(0) - 1 do
            for j = 0 to m.GetLength(1) - 1 do
                if m.[i, j] <> 0
                then k <- k + 1
        k
    (float (go m1 / (m1.GetLength(0)*m1.GetLength(1))), float (go m2 / (m2.GetLength(0)*m2.GetLength(1))))
