module hw12
open System.Threading.Tasks
open matrices

let genSparseMatrix<'t> (path: string) size quant (sparse: float) =    
    for l = 0 to quant - 1 do
        let m = Array2D.zeroCreate size size
        let a = int (round (float (size * size) * sparse))
        let mutable k = 0
        while k < a do
            let (i, j) = (new System.Random()).Next (0, size), (new System.Random()).Next (0, size)
            if typeof<'t> = typeof<int32>
            then
                if m.[i, j] = null
                then
                    m.[i, j] <- string ((new System.Random()).Next(1, 9))
                    k <- k + 1
            elif typeof<'t> = typeof<float>
            then
                if m.[i, j] = null
                then
                    m.[i, j] <- string (float ((new System.Random()).NextDouble()) * 10.0)
                    k <- k + 1
            elif typeof<'t> = typeof<bool>
            then
                if m.[i, j] = null
                then
                    m.[i, j] <- "1"
                    k <- k + 1
        let res = [| for i in 1 .. m.GetLength 1 -> "" |]
        for i = 0 to res.Length - 1 do
            for j = 0 to m.GetLength 0 - 1 do
               if m.[i, j] = null
               then
                   m.[i, j] <- "0"
               res.[i] <- res.[i] + m.[i, j] + " "
        System.IO.File.WriteAllLines(path.[0..path.Length - 5] + string l + path.[path.Length - 4..path.Length - 1] , res)

let multArr (a: int [,]) (b: int [,]) =
    if a.GetLength 0 = b.GetLength 1
    then
        let res = Array2D.zeroCreate (a.GetLength 0) (b.GetLength 1)
        for i = 0 to (a.GetLength 0 - 1) do
            for j = 0 to (b.GetLength 1 - 1) do
                for l = 0 to (a.GetLength 1 - 1) do
                    res.[i, j] <- res.[i, j] + a.[i, l]*b.[l, j]
        res
    else failwith "Size of matrices is not correct"


let parallelMult1 (m1: int [,]) (m2: int [,]) =    
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    Parallel.For(0, m1.GetLength 0, fun i ->
        for j = 0 to m2.GetLength 1 - 1 do
            for k = 0 to m1.GetLength 1 - 1 do
                res.[i, j] <- res.[i, j] + m1.[i, k]*m2.[k, j])
    |> ignore
    res
               
let parallelMult2 (m1: int [,]) (m2: int [,]) =    
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    for i = 0 to m1.GetLength 1 do
        Parallel.For(0, m1.GetLength 0, fun j ->
            for k = 0 to m1.GetLength 1 do
                res.[i, j] <- res.[i, j] + m1.[i, k]*m2.[k, j])
        |> ignore
    res
    
let parallelMult3 (m1: int [,]) (m2: int [,]) =    
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    for i = 0 to m1.GetLength 1 do
        for j = 0 to m2.GetLength 1 do
            Parallel.For(0, m1.GetLength 0, fun k ->            
                res.[i, j] <- res.[i, j] + m1.[i, k]*m2.[k, j])
            |> ignore
    res

let mmParallel (m1: int [,]) (m2: int [,]) =
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    [ for i in 0 .. m1.GetLength 0 - 1 ->
          async {                 
                      do
                          for j in 0 .. m2.GetLength 1 - 1 do
                              for k in 0 .. m1.GetLength 1 - 1 do
                                  res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
                }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    res      

let QTmultParallel (m1: QTwithSize) (m2: QTwithSize) (algStr: SemiRing<int>) p =
    let rec goMult (m1: QuadTree<int>) (m2: QuadTree<int>) (algStr: SemiRing<int>) c = 
        match (m1, m2) with
        |(Leaf a, Leaf b) ->
            if algStr.Mult a b = algStr.Neitral
            then None
            else Leaf (algStr.Mult a b)
        |(a, None) -> None
        |(Node(a1, a2, a3, a4), Node(b1, b2, b3, b4)) ->
            if c < p
            then
                let help x y z w = goAdd (goMult x y algStr (c + 1)) (goMult z w algStr (c + 1)) algStr 
                let NW = async {return help a1 b1 a2 b3}
                let NE = async {return help a1 b2 a2 b4}
                let SW = async {return help a3 b1 a4 b3}
                let SE = async {return help a3 b2 a4 b4}
                let s = [NW; NE; SW; SE] |> Async.Parallel |> Async.RunSynchronously
                mergeNone(s.[1], s.[2], s.[3], s.[4])
            else
                let help x y z w = goAdd (goMult x y algStr 1000) (goMult z w algStr 1000) algStr 
                let NW = help a1 b1 a2 b3
                let NE = help a1 b2 a2 b4
                let SW = help a3 b1 a4 b3
                let SE = help a3 b2 a4 b4
                mergeNone(NW, NE, SW, SE)
        |(_, Node(_, _, _, _)) |(_, Leaf _) -> failwith "Incorrected"
    if m1.colomns = m2.lines
    then QTwithSize (goMult m1.qt m2.qt algStr p, m1.lines, m2.colomns)
    else failwith "matrices can not be mult"
