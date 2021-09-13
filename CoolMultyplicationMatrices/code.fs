module code
open matrices
open helper

let semiRing = SemiRing<int>(0, (+), (*))

let listAllFiles dir =
    let files = System.IO.Directory.GetFiles(dir)
    List.ofArray files

type msg =
    | Go of AsyncReplyChannel<unit>
    | POM of string*string*int [,]*int [,]
    | EOS of AsyncReplyChannel<unit>

let mtrxLoader inDir (mtrxBalancer: MailboxProcessor<_>) quant =
    let mutable n = quant
    MailboxProcessor.Start(fun inbox ->
        let rec loop files =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Matrix loader is ready to finish!"
                    mtrxBalancer.PostAndReply (fun ch -> EOS ch)
                    printfn "Matrix loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        printfn "Matrix reading is finished!"
                        inbox.Post (EOS ch)
                        return! loop files
                    | file1 :: file2 :: files ->
                        if n <> 0
                        then
                            printfn "Load: %A and %A" file1 file2
                            let m1, m2 = read file1, read file2
                            mtrxBalancer.Post (POM (file1, file2, m1, m2))
                            n <- n - 1
                            inbox.Post (Go ch)
                            return! loop files
                        else
                            printfn "Matrix reading is finished!"
                            inbox.Post (EOS ch)
                            return! loop files
                    | [_] -> printfn "Error"
                | POM(_, _, _, _) -> printfn "Error"                    
            }
        loop (listAllFiles inDir)
        )

let mtrxBalancer (commonMult: MailboxProcessor<_>) (commonMultPar: MailboxProcessor<_>) (qMult: MailboxProcessor<_>) (qMultPar :MailboxProcessor<_>)=
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Balancer is ready to finish!"
                    commonMult.PostAndReply (fun ch -> EOS ch)
                    commonMultPar.PostAndReply (fun ch -> EOS ch)
                    qMult.PostAndReply (fun ch -> EOS ch)
                    qMultPar.PostAndReply (fun ch -> EOS ch)
                    printfn "Matrix balancer is finished!"
                    ch.Reply()
                | POM (file1, file2, m1, m2) ->
                    printfn "Processing %A and %A" file1 file2
                    let mutable sparce = false
                    let mutable size = false
                    if (fst (calcOfSparsity m1 m2) < 0.3) || (snd (calcOfSparsity m1 m2) < 0.3)
                    then sparce <- true 
                    if (m1.GetLength(0) > 1000) || (m2.GetLength(0) > 1000)
                    then size <- true
                    match(sparce, size) with
                    |(true, true) -> qMultPar.Post (POM (file1, file2, m1, m2))
                    |(true, false) -> qMult.Post (POM (file1, file2, m1, m2))
                    |(false, true) -> commonMultPar.Post (POM (file1, file2, m1, m2))
                    |(false, false) -> commonMult.Post (POM (file1, file2, m1, m2))
                    return! loop ()
                | Go ch -> printfn "Error"
                 }
        loop () 
        )

let commonMultSeq =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Common sequence mult is finished!"
                    ch.Reply()
                | POM (file1, file2, m1, m2) ->
                    let res = multMatrix m1 m2                  
                    return! loop ()
                | Go ch -> printfn "Error"     
            }
        loop ()
        )

let commonMultPar  =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Common parallel mult is finished!"
                    ch.Reply()
                | POM (file1, file2, m1, m2) ->
                    let res = multMatrixPar m1 m2                  
                    return! loop ()
                | Go ch -> printfn "Error"
            }
        loop ()
        )

let qMultSeq  =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "QuadTree sequence mult is finished!"
                    ch.Reply()
                | POM (file1, file2, m1, m2) ->
                    let qm1 = extQT m1
                    let qm2 = extQT m2
                    let res = QTmult qm1 qm2 semiRing
                    return! loop ()
                | Go ch -> printfn "Error"
            }
        loop ()
        )

let qMultPar  =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "QuadTree parallel mult is finished!"
                    ch.Reply()
                | POM (file1, file2, m1, m2) ->
                    let qm1 = extQT m1
                    let qm2 = extQT m2
                    let res = QTmultParallel qm1 qm2 semiRing
                    return! loop ()
                | Go ch -> printfn "Error"
            }
        loop ()
        )

let processSomeFilesAsync inDir n =
    let mtrxBalancer = mtrxBalancer commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer n
    mtrxLoader.PostAndReply(fun ch -> Go ch)

let processAllFilesAsync inDir =
    let mtrxBalancer = mtrxBalancer commonMultSeq commonMultPar qMultSeq qMultPar
    let mtrxLoader = mtrxLoader inDir mtrxBalancer (((listAllFiles inDir).Length) / 2)
    mtrxLoader.PostAndReply(fun ch -> Go ch)
