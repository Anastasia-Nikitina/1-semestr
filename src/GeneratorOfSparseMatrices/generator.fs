module generator

let rand = new System.Random()

let fill x (i, j) (m: _[,]) = 
    if m.[i, j] = null
    then
       m.[i, j] <- x
        
let genSparseMatrix<'t> (path: string) size quant (sparse: float) =    
    for l = 0 to quant - 1 do
        let m = Array2D.zeroCreate size size
        let a = int (round (float (size * size) * sparse))
        let mutable k = 0
        while k < a do
            let (i, j) = rand.Next (0, size), rand.Next (0, size)
            if typeof<'t> = typeof<int32>
            then
                fill (string (rand. Next(1, 9))) (i, j) m
                k <- k + 1
            elif typeof<'t> = typeof<float>
            then
                fill (string (float (rand.NextDouble()) * 10.0)) (i, j) m
                k <- k + 1
            elif typeof<'t> = typeof<bool>
            then
                fill "1" (i, j) m
                k <- k + 1
        let res = [| for i in 1 .. m.GetLength 1 -> "" |]
        for i = 0 to res.Length - 1 do
            for j = 0 to m.GetLength 0 - 1 do
               if m.[i, j] = null
               then
                   m.[i, j] <- "0"
               res.[i] <- res.[i] + m.[i, j] + " "
        System.IO.File.WriteAllLines(path.[0..path.Length - 5] + string l + path.[path.Length - 4..path.Length - 1] , res)
