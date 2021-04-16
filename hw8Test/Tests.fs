module Hw8Teests
open matrices
open Expecto

let semiRing = SemiRing<int>(0, (+), (*))

let genRandomArray  =
    let a = Array2D.zeroCreate 2 2
    for i = 0 to 1 do
        for j = 0 to 1 do
          a.[i, j] <- (new System.Random()).Next (-10, 10)
    a

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

let addArr (a: int [,]) (b: int [,]) =
    if (a.GetLength 0, a.GetLength 1) = (b.GetLength 0, b.GetLength 1)
    then
        let res = Array2D.zeroCreate (a.GetLength 0) (a.GetLength 1)
        for i = 0 to (a.GetLength 0 - 1) do
            for j = 0 to (a.GetLength 1 - 1) do
            res.[i, j] <- a.[i, j] + b.[i, j]
        res
    else failwith "Size of matrices is not correct"
    
let tensorMultArr (a: int [,]) (b: int [,]) =
    let m1 = a.GetLength 0
    let n1 = a.GetLength 1
    let m2 = b.GetLength 0
    let n2 = b.GetLength 1
    let res = Array2D.zeroCreate (m1 * m2) (n1 * n2)
    for i = 0 to m1 - 1 do
        for k = 0 to n1 - 1 do
            for j = 0 to m2 - 1 do
                for l = 0 to n2 - 1 do
                    res.[m2*i + j, n2*k + l] <- a.[i, k] * b.[j, l]
    res
    
[<Tests>]
let PropertyTests =
    testList "Property tests"
        [testProperty  "Comparision add QT with add arrays" <| fun _ ->
             let arr1 = genRandomArray             
             let arr2 = genRandomArray
             let m1 = TransformToQTwithSize arr1 (arr1.GetLength 0) (arr1.GetLength 1)
             let m2 = TransformToQTwithSize arr2 (arr2.GetLength 0) (arr2.GetLength 1)
             let expect = (TransformToQTwithSize (addArr arr1 arr2) (arr1.GetLength 0) (arr1.GetLength 1)).qt
             let res = (add m1 m2 semiRing).qt 
             Expect.equal res expect
             
         testProperty  "Comparision mult QT with mult arrays" <| fun _ ->
             let arr1 = genRandomArray
             let arr2 = genRandomArray
             let m1 = TransformToQTwithSize arr1 (arr1.GetLength 0) (arr1.GetLength 1)
             let m2 = TransformToQTwithSize arr2 (arr2.GetLength 0) (arr2.GetLength 1)
             let expect = (TransformToQTwithSize (multArr arr1 arr2) (arr1.GetLength 0) (arr1.GetLength 1)).qt
             let res = (mult m1 m2 semiRing).qt
             Expect.equal res expect

         testProperty  "Comparision tensor mult QT with tensor mult arrays" <| fun _ ->
             let arr1 = genRandomArray
             let arr2 = genRandomArray
             let m1 = TransformToQTwithSize arr1 (arr1.GetLength 0) (arr1.GetLength 1)
             let m2 = TransformToQTwithSize arr2 (arr2.GetLength 0) (arr2.GetLength 1)
             let expect = (TransformToQTwithSize (tensorMultArr arr1 arr2) (arr1.GetLength 0) (arr1.GetLength 1)).qt
             let res = (tensorMult m1 m2 semiRing).qt
             Expect.equal res expect
        ]

