module MyTree
open MyList
open System

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>
      
let rec fold f acc tree =
    match tree with
    | Leaf x -> f acc x
    | Node (head, tail) -> MyList.fold (fun acc x -> fold f acc x) (f acc head) tail

let getMaximum tree =
    fold (fun m x -> if x > m then x else m) System.Int32.MinValue tree

let getAverage tree =
    let sum = fold (fun sum x -> sum + x) 0 tree
    let quantity = fold (fun q x  -> q + 1) 0 tree
    float sum / float quantity
    
