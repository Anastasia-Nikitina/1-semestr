module hw6
open System.IO

[<Measure>] type _line
[<Measure>] type _colomn

[<Struct>]
type Coordinates =
     val X: int<_line>
     val Y: int<_colomn>
     new(x, y) = {X=x; Y=y}
     
[<Struct>]
type BoolMatrix =
    val Length: int
    val Width: int
    val Coordinate: list<Coordinates>
    new(m, n, c) = {Length = m; Width= n; Coordinate = c}

let readBoolMatrix file =
    let matrix = File.ReadAllLines file
    let mutable k = 0
    let mutable i = 1
    while (k = 0) && (i < matrix.Length) do
        if matrix.[0].Length <> matrix.[i].Length       
        then
            k <- k + 1
        i <- i + 1
    if k = 0 
    then
        let list =
            [for i = 0 to matrix.Length - 1 do
                for j = 0 to matrix.[0].Length - 1 do   
                    if matrix.[i].[j] = '1'
                    then new Coordinates(i*1<_line>, j*1<_colomn>)
            ]   
        new BoolMatrix (matrix.Length, matrix.[0].Length, list)
    else failwith "Size of matrix is not correct"

let multBoolMatrix (m1: BoolMatrix, m2: BoolMatrix) =
    if m1.Width <> m2.Length
    then failwith "Matrices can not be multiplicated"
    else
        let resCoordinates =
            [ for i in m1.Coordinate do
                for j in m2.Coordinate do
                    if int i.Y = int j.X then new Coordinates (i.X, j.Y)
            ]
        new BoolMatrix (m1.Length, m2.Width, List.distinct resCoordinates)
              
let BoolMatrixToArray (matrix: BoolMatrix) =
    let m = List.toArray matrix.Coordinate
    let resMatrix = Array2D.zeroCreate matrix.Length matrix.Width
    for i = 0 to m.Length - 1 do
        resMatrix.[int m.[i].X, int m.[i].Y] <- 1
    resMatrix

let array2DIntoString (array: int[,]) =
    let stringArr: string [] = Array.zeroCreate (array.GetLength 0) 
    for i = 0 to (array.GetLength 0 - 1) do
        for j = 0 to (array.GetLength 0 - 1) do
            stringArr.[i] <- stringArr.[i] + string array.[i, j]
    stringArr

let writeArray2D file (array: int[,]) =
    File.WriteAllLines (file, array2DIntoString array)
