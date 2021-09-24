open System
open matrices


let semiRing = SemiRing<int>(0, (+), (*))

let rand = new System.Random()

let genRandomArray m  =    
    let a = Array2D.zeroCreate m m
    for i = 0 to a.GetLength 0 - 1 do
        for j = 0 to a.GetLength 1 - 1  do
          a.[i, j] <- rand.Next (-10, 10)
    a

[<EntryPoint>]
let main(argv: array<string>) =
  let arr1 = genRandomArray 3         
  let arr2 = genRandomArray 3       
  let x = (arr1.GetLength 0)*(arr2.GetLength 1)     
  let m1 = extQT arr1
  let m2 = extQT arr2
  let seq = QTmult m1 m2 semiRing            
  let par = QTmultParallel m1 m2 semiRing 2   
  printfn "%A" seq.qt
  printfn "%A" par.qt
  0
