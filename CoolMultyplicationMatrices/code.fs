module code
open matrices
open helper

let semiRing = SemiRing<int>(0, (+), (*))
    
let listAllFiles dir =
    let files = System.IO.Directory.GetFiles(dir)
    List.ofArray files

[<Struct>] 
type cnst =
    val sparsity: float
    val size: int
    val algStr: SemiRing<int>
    val par: int
    new(a, b, s, p) = {sparsity = a; size = b; algStr = s; par = p}

[<Struct>]    
type PairOfMatrices =
        val path1: string
        val path2: string
        val m1: int[,]
        val m2: int[,]
        val isBig: bool
        val isSparse: bool
        new (f1, f2, m1, m2, a, b)  = {path1 = f1; path2 = f2; m1 = m1; m2 = m2; isBig = a; isSparse = b} 

type msgBalancer =
    | EOS of AsyncReplyChannel<unit>
    | Matrices of PairOfMatrices

type msgLoader =
    | EOS of AsyncReplyChannel<unit> 
    | Go of AsyncReplyChannel<unit>
 
let mtrxLoader inDir (mtrxBalancer: MailboxProcessor<msgBalancer>) quant =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files n =
            async{
                let! (msg: msgLoader) = inbox.Receive()
                match msg with
                | msgLoader.EOS ch ->
                    printfn "Matrix loader is ready to finish!"
                    mtrxBalancer.PostAndReply msgBalancer.EOS
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        printfn "Matrix reading is finished!"
                        inbox.Post (msgLoader.EOS ch)
                        return! loop files n
                    | file1 :: file2 :: files ->
                        if n > 0
                        then
                            printfn "Load: %A and %A" file1 file2
                            let m1, m2 = read file1, read file2
                            mtrxBalancer.Post (Matrices (PairOfMatrices(file1, file2, m1, m2, false, false)))
                            inbox.Post (Go ch)
                            return! loop files (n - 1)
                        else
                            printfn "Matrix reading is finished!"
                            inbox.Post (msgLoader.EOS ch)
                            return! loop files (n - 1)
                    | [_] -> printfn "Error"                    
            }
        loop (listAllFiles inDir) quant
        )

let mtrxBalancer (k: cnst) (commonMult: MailboxProcessor<msgBalancer>) (commonMultPar: MailboxProcessor<msgBalancer>) (qMult: MailboxProcessor<msgBalancer>) (qMultPar: MailboxProcessor<msgBalancer>)=
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | msgBalancer.EOS ch ->
                    printfn "Balancer is ready to finish!"
                    commonMult.PostAndReply (fun ch ->  msgBalancer.EOS ch)
                    commonMultPar.PostAndReply (fun ch ->  msgBalancer.EOS ch)
                    qMult.PostAndReply (fun ch ->  msgBalancer.EOS ch)
                    qMultPar.PostAndReply (fun ch ->  msgBalancer.EOS ch)
                    printfn "Matrix balancer is finished!"
                    ch.Reply()
                | Matrices(pair) ->                                    
                    let s1 = fst (calcOfSparsity pair.m1 pair.m2)
                    let s2 = snd (calcOfSparsity pair.m1 pair.m2)
                    let b1 = pair.m1.GetLength(0)
                    let b2 = pair.m2.GetLength(0)
                    printfn "Processing %A and %A" pair.path1 pair.path2
                    if (s1 < k.sparsity || s2 < k.sparsity) && (b1 > k.size || b2 > k.size) 
                    then qMult.Post (Matrices (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, true)))
                    elif (s1 < k.sparsity || s2 < k.sparsity) && (b1 <= k.size && b2 <=k.size)
                    then qMult.Post (Matrices (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, false)))
                    elif (s1 >= k.sparsity && s2 >= k.sparsity) && (b1 > k.size || b2 > k.size)
                    then commonMult.Post (Matrices (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, true)))
                    else commonMult.Post (Matrices (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, false)))
                    return! loop ()
                 }
        loop () 
        )

let QTmultSeqFix m1 m2 s n =
    QTmult m1 m2 s

let Qmult multfun s n (prnt: string) =
    MailboxProcessor.Start(fun inbox ->
           let rec loop () =
               async{
                   let! (msg: msgBalancer) = inbox.Receive()
                   match msg with
                   | msgBalancer.EOS ch ->
                       printfn "Mult is finished!"
                       ch.Reply()
                       return! loop ()
                   | Matrices pair  ->
                       printfn "%A" prnt
                       let res = multfun (extQT pair.m1) (extQT pair.m2) s n
                       return! loop ()
                   }
           loop ()
           )
       
let ArrMult multfun (prnt: string) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! (msg: msgBalancer) = inbox.Receive()
                match msg with
                | msgBalancer.EOS ch ->
                    printfn "Mult is finished!"
                    ch.Reply()
                    return! loop ()
                | Matrices p ->
                    printfn "%A" prnt
                    let res =  multfun p.m1 p.m2
                    return! loop ()        
                }
        loop ()
        )

let processSomeFilesAsync inDir (k: cnst) =
    let (commonMultSeq, commonMultPar) = (ArrMult multMatrix "Sequence array mult", ArrMult multMatrixPar "Parallel array mult")
    let (qMultSeq, qMultPar) = (Qmult QTmultSeqFix k.algStr k.par "Parallel QT mult", Qmult QTmultParallel k.algStr k.par "Parallel QT mult")
    let mtrxBalancer = mtrxBalancer k commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer k.par
    mtrxLoader.PostAndReply Go

let processAllFilesAsync inDir (k: cnst) =
    let (commonMultSeq, commonMultPar) = (ArrMult multMatrix "Sequence array mult", ArrMult multMatrixPar "Parallel array mult")
    let (qMultSeq, qMultPar) = (Qmult QTmultSeqFix  k.algStr k.par "Parallel QT mult", Qmult QTmultParallel k.algStr k.par "Parallel QT mult")
    let mtrxBalancer = mtrxBalancer k commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer (((listAllFiles inDir).Length) / 2)
    mtrxLoader.PostAndReply Go
