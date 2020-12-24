module MyTree
open MyList
open System

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>

let rec Fold f acc tree =
    match tree with
    | Leaf x -> f acc x
    | Node (head, tail) -> MyList.Fold (fun acc x -> Fold f acc x) (f acc head) tail

let getMaximum tree =
    Fold max System.Int32.MinValue tree

let getAverage tree =
    let (sum, quantity) = Fold (fun (sum, q) x -> sum + x, q + 1) (0, 0) tree
    float sum / float quantity
