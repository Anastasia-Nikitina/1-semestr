module matrices
open System.IO

(*type QuadTree<'t> =
    | NorthWest of 't
    | NorthEast of 't
    | SouthWest of 't
    | SouthEast of 't
    


let extension (m: string[]) a =    
    let matrix1: int [,] = Array2D.zeroCreate a a 
    for i = 0 to m.Length - 1 do
        for j = 0 to m.[0].Length - 1 do
            matrix1.[i, j] <- int m.[i].[j]
    matrix1

let readMatrix file =
    let matrix = File.ReadAllLines file
    let mutable size = matrix.Length * matrix.[0].Length
    let mutable i = 0
   

    while (size <> 1) && (size % 2 = 0) do
        i <- i + 1
        size <- size / 2
    if size = 1
    then
        if i % 2 = 0
        then extension matrix matrix.Length //все ок, переводим матрицу в дерево квадрантов
        else extension matrix (max matrix.Length matrix.[0].Length) //норм. переводим matrix1 в дерево квадрантов 
    else        
        let mutable x = ceil ((log10  (float (max matrix.Length matrix.[0].Length)) ** 2.0 ) / (log10 2.0))
        //ищем ближайшую степень двойки
        if x % 2.0 <> 0.0 then x <- x + 1.0
        extension matrix (int (sqrt (2.0 ** x)))//теперь норм
        

        
0*)
