module matrices
open System.IO
open System.Threading.Tasks

[<Struct>]
type SemiRing<'t> =  
        val Neitral: 't
        val Add: 't -> 't -> 't   
        val Mult: 't -> 't -> 't
        new (n, x, y) = {Neitral = n; Add = x; Mult = y}
    
type QuadTree<'t> =    
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

[<Struct>]    
type QTwithSize =
        val qt: QuadTree<int>
        val lines: int
        val colomns: int
        new (qt, a, b)  = {qt = qt; lines = a; colomns = b} 
    
let Size (m: int [,])  =
    let x = m.GetLength(1) * m.GetLength(0)
    x

let mergeNone (NW, NE, SW, SE) =
    if (NW, NE, SW, SE) = (None, None, None, None)
    then None
    else Node(NW, NE, SW, SE)

let TransformToQTwithSize (m: int [,]) g h =
    let rec go (m: int [,]) = 
        if Size m = 0
        then None
        elif Size m = 1
        then
            if m.[0, 0] = 0
            then None
            else Leaf (m.[0, 0])
        else
           let NW = go m.[0 .. m.GetLength(0) / 2 - 1, 0 .. m.GetLength(0) / 2 - 1]
           let NE = go m.[0 .. m.GetLength(0) / 2 - 1,  m.GetLength(0) / 2 .. m.GetLength(0) - 1]
           let SW = go m.[m.GetLength(0) / 2 .. m.GetLength(0) - 1, 0 .. m.GetLength(0) / 2 - 1]
           let SE = go m.[m.GetLength(0) / 2 .. m.GetLength(0) - 1, m.GetLength(0) / 2 .. m.GetLength(0) - 1]
           mergeNone (NW, NE, SW, SE)
    QTwithSize (go m, g, h)

let read file =   
    let m = File.ReadAllLines file
    let x = m.Length
    let mutable str = m.[0]
    for i = 1 to m.Length - 1 do
        str <- str + " " + m.[i]
    let numbers = str.Split(' ')    
    let y = (numbers.Length - x) / x    
    let arr = Array2D.zeroCreate x y 
    let mutable k = 0
    for i = 0 to x - 1 do
        for j = 0 to y - 1 do           
            if numbers.[k] = ""
            then k <- k + 1                       
            arr.[i, j] <- int (numbers.[k])          
            k <- k + 1
    arr
        
let extArr (a: int [,]) =
    let x = a.GetLength 0
    let y = a.GetLength 1
    let mutable size = a.Length
    let mutable i = 0  
    while (size <> 1) && (size % 2 = 0) do
        i <- i + 1
        size <- size / 2   
    if size = 1
    then
        let (arr: int[,])= Array2D.zeroCreate (max x y) (max x y)
        for i = 0 to a.GetLength 0 - 1 do
            for j = 0 to a.GetLength 1 - 1 do
                arr.[i, j] <-  a.[i, j] 
        arr
    else        
        let mutable k = ceil ((log10  (float (max x y) ** 2.0 )) / (log10 2.0)) // ищем ближайшую степень двойки
        if k % 2.0 <> 0.0 then k <- k + 1.0
        let m = int (sqrt (2.0**k))
        let (arr: int[,])= Array2D.zeroCreate m m
        for i = 0 to a.GetLength 0 - 1 do
            for j = 0 to a.GetLength 1 - 1 do               
                arr.[i, j] <-  a.[i, j]
        arr

let extQT (a: int [,]) =
    TransformToQTwithSize (extArr a) (a.GetLength 0) (a.GetLength 0)
            
let rec goAdd (m1: QuadTree<int>) (m2: QuadTree<int>) (algStr: SemiRing<int>) =    
    match (m1, m2) with    
    |(a, None) |(None, a) -> a
    |(Leaf a, Leaf b) ->
        if algStr.Add a b = algStr.Neitral
        then None
        else Leaf(algStr.Add a b)
    |(Node(one_1, two_1, three_1, four_1), Node(one_2, two_2, three_2, four_2)) ->
        let NW = goAdd one_1 one_2 algStr
        let NE = goAdd two_1 two_2 algStr
        let SW = goAdd three_1 three_2 algStr
        let SE = goAdd four_1 four_2 algStr
        mergeNone(NW, NE, SW, SE)
    |(Node(_, _, _, _), _) |(Leaf _, _)  -> failwith "Incorrect"
    
let add (m1: QTwithSize) (m2: QTwithSize) (algStr: SemiRing<int>) =   
    if m1.lines = m2.lines && m1.colomns = m2.colomns
    then QTwithSize (goAdd m1.qt m2.qt algStr, m1.lines, m1.colomns)
    else failwith "matrices can not be add"

let QTmult (m1: QTwithSize) (m2: QTwithSize) (algStr: SemiRing<int>) =
    let rec goMult (m1: QuadTree<int>) (m2: QuadTree<int>) (algStr: SemiRing<int>) =       
        match (m1, m2) with
        |(Leaf a, Leaf b) ->
            if algStr.Mult a b = algStr.Neitral
            then None
            else Leaf (algStr.Mult a b)
        |(a, None) |(None, a) -> None
        |(Node(a1, a2, a3, a4), Node(b1, b2, b3, b4)) ->
            let NW = goAdd (goMult a1 b1 algStr) (goMult a2 b3 algStr) algStr
            let NE = goAdd (goMult a1 b2 algStr) (goMult a2 b4 algStr) algStr
            let SW = goAdd (goMult a3 b1 algStr) (goMult a4 b3 algStr) algStr
            let SE = goAdd (goMult a3 b2 algStr) (goMult a4 b4 algStr) algStr
            mergeNone(NW, NE, SW, SE)
        |(_, Node(_, _, _, _)) |(_, Leaf _) -> failwith "Incorrect"
    if m1.colomns = m2.lines
    then QTwithSize (goMult m1.qt m2.qt algStr, m1.lines, m2.colomns)
    else failwith "matrices can not be mult"

let rec numberToMatrix (n: int) (m: QuadTree<int>) (algStr: SemiRing<int>) =
    let neitral = algStr.Neitral
    let operation = algStr.Mult    
    let rec go (n: int) (m: QuadTree<int>) =
        if n = neitral
        then None
        else
            match m with
            | None -> None
            | Leaf x ->
                if operation n x = neitral
                then None
                else Leaf (operation n x)
            | Node (a, b, c, d) -> Node (go n a , go n b , go n c, go n d)
    go n m
  
let tensorMult1 (m1: QTwithSize) (m2: QTwithSize) (algStr: SemiRing<int>) =
   let rec go (m1: QuadTree<int>) (m2: QuadTree<int>) =
        match (m1, m2) with
        |(Leaf x, a) -> numberToMatrix x a algStr
        |(Node(a, b, c, d), Leaf x) |(Leaf x, Node(a, b, c, d)) -> Node (numberToMatrix x a algStr, numberToMatrix x b algStr, numberToMatrix x c algStr, numberToMatrix x d algStr)
        |(Node(a, b, c, d), Node(x, y, z, w)) ->
            let NW = go a (Node(x, y, z, w))
            let NE = go b (Node(x, y, z, w))
            let SW = go c (Node(x, y, z, w))
            let SE = go d (Node(x, y, z, w))
            mergeNone(NW, NE, SW, SE)
        |(a, None)|(None, a) -> None  
   QTwithSize (go m1.qt m2.qt, (m1.lines * m2.lines), (m1.colomns * m2.colomns))

let QTmultParallel (m1: QTwithSize) (m2: QTwithSize) (algStr: SemiRing<int>) p =
    let rec goMult (m1: QuadTree<int>) (m2: QuadTree<int>) (algStr: SemiRing<int>) c =
        match (m1, m2) with
        |(Leaf a, Leaf b) ->
            if algStr.Mult a b = algStr.Neitral
            then None
            else Leaf (algStr.Mult a b)
        |(_, None) -> None |(None, _) -> None
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
