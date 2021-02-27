module matrices
open System.IO

type QuadTree<'t> =    
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>
    
type QTwithSize =
    val qt: QuadTree<int>
    val lines: int
    val colomns: int
    new (qt, a, b)  = {qt = qt; lines = a; colomns = b}

let Size (m: int [,])  =
    let x = m.GetLength(1) * m.GetLength(0)
    x

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
            let x = m.GetLength(0) / 2
            let NW = Array2D.zeroCreate x x
            let NE = Array2D.zeroCreate x x        
            let SW = Array2D.zeroCreate x x        
            let SE = Array2D.zeroCreate x x        
            for i = 0 to m.GetLength(0) / 2 - 1 do
                for j = 0 to m.GetLength(0) / 2 - 1 do             
                    NW.[i, j] <- m.[i, j]
            for i = 0 to m.GetLength(0) / 2 - 1  do 
                for j = m.GetLength(0) / 2 to m.GetLength(0) - 1  do                
                    NE.[i, j - m.GetLength(0) / 2] <- m.[i, j]
            for i = m.GetLength(0) / 2 to m.GetLength(0) - 1  do
                for j = 0 to m.GetLength(0) / 2 - 1 do               
                    SW.[i - m.GetLength(0) / 2 , j] <- m.[i, j]
            for i = m.GetLength(0) / 2 to m.GetLength(0) - 1 do
                for j = m.GetLength(0) / 2 to m.GetLength(0) - 1 do                
                    SE.[i - m.GetLength(0) / 2, j - m.GetLength(0) / 2] <- m.[i, j]            
            Node (go NW, go NE, go SW, go SE)
    QTwithSize (go m, g, h)   

let readAndExtension file =   
    let m = File.ReadAllLines file
    let x = m.Length
    let mutable str = m.[0]
    for i = 1 to m.Length - 1 do
        str <- str + " " + m.[i]
    let numbers = str.Split(' ')
    let y = numbers.Length / x
    let mutable size = numbers.Length
    let mutable i = 0  
    while (size <> 1) && (size % 2 = 0) do
        i <- i + 1
        size <- size / 2
    if size = 1
    then
        let (arr: int[,])= Array2D.zeroCreate (max x y) (max x y)
        for i = 0 to numbers.Length - 1 do
            for j = numbers.[i].Length - 1 downto 0 do
                arr.[i / y, i % y] <-  arr.[i / y, i % y] + (int numbers.[i].[j] - 48) * int (10.0 ** float (numbers.[i].Length - 1 - j))
        TransformToQTwithSize arr x y
    else        
        let mutable k = ceil ((log10  (float (max x y)) ** 2.0 ) / (log10 2.0)) // ищем ближайшую степень двойки
        if k % 2.0 <> 0.0 then k <- k + 1.0       
        let (arr: int[,])= Array2D.zeroCreate (int (2.0 ** k)) (int (2.0 ** k))
        for i = 0 to numbers.Length - 1 do
            for j = numbers.[i].Length - 1 downto 0 do
                arr.[i / y, i % y] <-  arr.[i / y, i % y] + (int numbers.[i].[j] - 48) * int (10.0 ** float (numbers.[i].Length - 1 - j))
        TransformToQTwithSize arr x y  

let rec goAdd (m1: QuadTree<_>) (m2: QuadTree<_>) = 
    match (m1, m2) with
    |(None, None) -> None
    |(Leaf a, Leaf b) -> Leaf(a + b)
    |(Leaf a, None) -> Leaf a
    |(None, Leaf a) -> Leaf a
    |(Node(one_1, two_1, three_1, four_1), Node(one_2, two_2, three_2, four_2)) ->
        Node (goAdd one_1 one_2, goAdd two_1 two_2, goAdd three_1 three_2, goAdd four_1 four_2)

let rec goMult (m1: QuadTree<_>) (m2: QuadTree<_>) =
    match (m1, m2) with
    |(Leaf a, Leaf b) -> Leaf(a * b)
    |(None, None) -> None
    |(None, Leaf _) -> None
    |(Leaf _, None) -> None
    |(Node(one_1, two_1, three_1, four_1), Node(one_2, two_2, three_2, four_2)) ->
        Node (goAdd (goMult one_1 one_2) (goMult two_1 three_2),goAdd (goMult one_1 two_2) (goMult two_1 four_2),
        goAdd (goMult three_1 one_2) (goMult four_1 three_2), goAdd (goMult three_1 two_2) (goMult four_1 four_2))
    
let add (m1: QTwithSize) (m2: QTwithSize) =
    if m1.lines = m2.lines && m1.colomns = m2.colomns
    then QTwithSize (goAdd m1.qt m2.qt, m1.lines, m1.colomns)
    else failwith "matrices can not be add"

let mult (m1: QTwithSize) (m2: QTwithSize) =
    if m1.colomns = m2.lines
    then QTwithSize (goMult m1.qt m2.qt, m1.lines, m2.colomns)
    else failwith "matrices can not be mult"

0
