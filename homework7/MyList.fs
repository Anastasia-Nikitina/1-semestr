module MyList

type MyList<'t> =
    | Single of 't
    | Cons of 't * MyList<'t>

let rec fold f acc list =
    match list with
    | Single x -> f acc x
    | Cons (head, tail) -> fold f (f acc head) tail

let Length list =
    fold (fun q x -> q + 1) 0 list

let rec Map f list =
    match list with
    | Single a -> Single (f a)
    | Cons (head, tail) -> Cons(f head, Map f tail)

let rec Iter f list =
    match list with
    | Single a -> f a
    | Cons (head, tail) ->
        f head
        Iter f tail

let rec Concat list1 list2 =
    match list1 with
    | Single x -> Cons(x, list2)
    | Cons (head, tail) -> Cons (head, Concat tail list2)
    
let rec TransformToMyList list =
    match list with
    | [] -> failwith "Empty list"
    | [a] -> Single a
    | head :: tail -> Cons (head, TransformToMyList tail)

let sort list =
    let rec go1 list1 =
        match list1 with
        | Single x -> Single x
        | Cons (head1, Cons(head2, tail)) ->
            if head1 >= head2
            then Cons (head2, go1 (Cons(head1, tail)))
            else Cons (head1, go1 (Cons(head2, tail)))
        | Cons (head, Single tail) ->
            if head >= tail
            then Cons (tail, Single head)
            else list1
    let rec go2 (list2: MyList<'t>) k =
        if k <> Length list2
        then go2 (go1 list2) (k + 1)
        else list2
    go2 list 0

