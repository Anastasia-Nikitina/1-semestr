module code
open matrices
open helper

let semiRing = SemiRing<int>(0, (+), (*))

let listAllFiles dir =
    let files = System.IO.Directory.GetFiles(dir)
    List.ofArray files

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
    | EOSb of AsyncReplyChannel<unit>
    | Matrices of PairOfMatrices

type msgLoader =
    | EOSl of AsyncReplyChannel<unit> 
    | Go of AsyncReplyChannel<unit>

type msgMult =
    | CommonSeq of PairOfMatrices
    | CommonPar of PairOfMatrices
    | QTseq of PairOfMatrices
    | QTpar of PairOfMatrices
    | EOSm of AsyncReplyChannel<unit>
  
let mtrxLoader inDir (mtrxBalancer: MailboxProcessor<msgBalancer>) quant =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files n =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSl ch ->
                    printfn "Matrix loader is ready to finish!"
                    mtrxBalancer.PostAndReply (fun ch -> EOSb ch)
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        printfn "Matrix reading is finished!"
                        inbox.Post (EOSl ch)
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
                            inbox.Post (EOSl ch)
                            return! loop files (n - 1)
                    | [_] -> printfn "Error"                    
            }
        loop (listAllFiles inDir) quant
        )

let mtrxBalancer (commonMult: MailboxProcessor<msgMult>) (commonMultPar: MailboxProcessor<msgMult>) (qMult: MailboxProcessor<msgMult>) (qMultPar: MailboxProcessor<msgMult>)=
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOSb ch ->
                    printfn "Balancer is ready to finish!"
                    commonMult.PostAndReply (fun ch -> EOSm ch)
                    commonMultPar.PostAndReply (fun ch -> EOSm ch)
                    qMult.PostAndReply (fun ch -> EOSm ch)
                    qMultPar.PostAndReply (fun ch -> EOSm ch)
                    printfn "Matrix balancer is finished!"
                    ch.Reply()
                | Matrices(pair) ->                                    
                    let s1 = fst (calcOfSparsity pair.m1 pair.m2)
                    let s2 = snd (calcOfSparsity pair.m1 pair.m2)
                    let b1 = pair.m1.GetLength(0)
                    let b2 = pair.m2.GetLength(0)
                    printfn "Processing %A and %A" pair.path1 pair.path2
                    if (s1 < 0.3 || s2 < 0.3) && (b1 > 1000 || b2 > 1000) 
                    then qMult.Post (QTpar (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, true)))
                    elif (s1 < 0.3 || s2 < 0.3) && (b1 <= 1000 && b2 <= 1000)
                    then qMult.Post (QTseq (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, false)))
                    elif (s1 >= 0.3 && s2 >= 0.3) && (b1 > 1000 || b2 > 1000)
                    then commonMult.Post (CommonPar (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, true)))
                    else commonMult.Post (CommonSeq (PairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, false)))
                    return! loop ()
                 }
        loop () 
        )

let mult =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! (msg: msgMult) = inbox.Receive()
                match msg with
                | EOSm ch ->
                    printfn "Mult is finished!"
                    ch.Reply()
                    return! loop ()
                | QTpar p ->
                    printfn "Parallel QT mult"
                    let res =  QTmultParallel (extQT p.m1) (extQT p.m2)   
                    return! loop ()
                | QTseq p ->
                    printfn "Sequence QT mult"
                    let res = QTmult (extQT p.m1) (extQT p.m2)    
                    return! loop ()
                | CommonSeq p ->
                    printfn "Sequence arr mult"
                    let res = multMatrix  p.m1 p.m2    
                    return! loop ()
                | CommonPar p ->
                    printfn "Parallel arr mult"
                    let res = multMatrixPar p.m1 p.m2  
                    return! loop ()                   
                }
        loop ()
        )

let processSomeFilesAsync inDir n =
    let (commonMultSeq, commonMultPar, qMultSeq, qMultPar) = (mult, mult, mult, mult)
    let mtrxBalancer = mtrxBalancer commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer n
    mtrxLoader.PostAndReply(fun ch -> Go ch)

let processAllFilesAsync inDir =
    let (commonMultSeq, commonMultPar, qMultSeq, qMultPar) = (mult, mult, mult, mult)
    let mtrxBalancer = mtrxBalancer commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer (((listAllFiles inDir).Length) / 2)
    mtrxLoader.PostAndReply(fun ch -> Go ch)
