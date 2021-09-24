module code
open matrices
open helper

let semiRing = SemiRing<int>(0, (+), (*))
    
let listFiles dir =
    let files = System.IO.Directory.GetFiles(dir)
    List.ofArray files

type parameters<'t> =
    val sparsity: float
    val size: int
    val algStr: SemiRing<'t>
    val par: int
    new(a, b, s, p) = {sparsity = a; size = b; algStr = s; par = p }

[<Struct>]    
type pairOfMatrices =
        val path1: string
        val path2: string
        val m1: int[,]
        val m2: int[,]
        val isBig: bool
        val isSparse: bool
        new (f1, f2, m1, m2, size, sparse)  = {path1 = f1; path2 = f2; m1 = m1; m2 = m2; isBig = size; isSparse = sparse} 

type msgBalancer =
    | EOS of AsyncReplyChannel<unit>
    | Matrices of pairOfMatrices

type msgLoader =
    | EOS of AsyncReplyChannel<unit> 
    | Go of AsyncReplyChannel<unit>
 
let mtrxLoader inDir (mtrxBalancer: MailboxProcessor<msgBalancer>) count =
    MailboxProcessor.Start(fun inbox ->
        let rec loop (files: string list)  =
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
                        return! loop files 
                    | file1 :: file2 :: files ->                                           
                         printfn "Load: %A and %A" file1 file2
                         let m1, m2 = read file1, read file2
                         mtrxBalancer.Post (Matrices (pairOfMatrices(file1, file2, m1, m2, false, false)))
                         inbox.Post (Go ch)
                         return! loop files
                    | [_] -> printfn "Error"                    
            }
        let files = (listFiles inDir).[..count*2 - 1]
        loop files
        )

let mtrxBalancer (prm: parameters<'t>) (commonMult: MailboxProcessor<msgBalancer>) (commonMultPar: MailboxProcessor<msgBalancer>) (qMult: MailboxProcessor<msgBalancer>) (qMultPar: MailboxProcessor<msgBalancer>)=
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | msgBalancer.EOS ch ->
                    printfn "Balancer is ready to finish!"
                    commonMult.PostAndReply msgBalancer.EOS
                    commonMultPar.PostAndReply msgBalancer.EOS
                    qMult.PostAndReply msgBalancer.EOS
                    qMultPar.PostAndReply  msgBalancer.EOS
                    printfn "Matrix balancer is finished!"
                    ch.Reply()
                | Matrices pair ->                                    
                    let s1 = fst (calcOfSparsity pair.m1 pair.m2)
                    let s2 = snd (calcOfSparsity pair.m1 pair.m2)
                    let b1 = pair.m1.GetLength(0)
                    let b2 = pair.m2.GetLength(0)
                    printfn "Processing %A and %A" pair.path1 pair.path2
                    if (s1 < prm.sparsity || s2 < prm.sparsity) && (b1 > prm.size || b2 > prm.size) 
                    then qMult.Post (Matrices (pairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, true)))
                    elif (s1 < prm.sparsity || s2 < prm.sparsity) && (b1 <= prm.size && b2 <=prm.size)
                    then qMult.Post (Matrices (pairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, true, false)))
                    elif (s1 >= prm.sparsity && s2 >= prm.sparsity) && (b1 > prm.size || b2 > prm.size)
                    then commonMult.Post (Matrices (pairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, true)))
                    else commonMult.Post (Matrices (pairOfMatrices (pair.path1, pair.path2, pair.m1, pair.m2, false, false)))
                    return! loop ()
                 }
        loop () 
        )

let QTmultSeqFix m1 m2 str n =
    QTmult (extQT m1) (extQT m2) str

let arrMultSeqFix m1 m2 str n =
    multMatrix m1 m2

let arrMultParFix m1 m2 str n =
    multMatrixPar m1 m2

let QTmultParFix m1 m2 str n =
    QTmultParallel (extQT m1) (extQT m2) str n
       
let templateMult multfun (prm: parameters<'t>) (prnt: string) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! (msg: msgBalancer) = inbox.Receive()
                match msg with
                | msgBalancer.EOS ch ->
                    printfn "Mult is finished!"
                    ch.Reply()
                    return! loop ()
                | Matrices pair ->
                    printfn "%A" prnt
                    let res =  multfun pair.m1 pair.m2 prm.algStr prm.par
                    return! loop ()        
                }
        loop ()
        )

let fixParameters = parameters(0.5, 1000, semiRing, 1)

let processFilesAsync prm inDir count =
    let (commonMultSeq, commonMultPar) = (templateMult arrMultSeqFix prm "Sequence array mult", templateMult arrMultParFix prm "Parallel array mult")
    let (qMultSeq, qMultPar) = (templateMult QTmultSeqFix prm "Parallel QT mult", templateMult QTmultParFix prm "Parallel QT mult")
    let mtrxBalancer = mtrxBalancer prm commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer count
    mtrxLoader.PostAndReply Go
