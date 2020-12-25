module hw7Tests
open MyList
open MyString
open MyTree
open Expecto

[<Tests>]
let PropertyTestsForLists =
    testList "TestPropertyForLists"
        [testProperty  "Comparision my sort with system sort" <| fun (list: list<int>) ->
            if not list.IsEmpty
            then
                Expect.sequenceEqual (TransformToSystemList (Sort (TransformToMyList list)))  (List.sort list)
            else
                Expect.sequenceEqual list []
         testProperty "Comparision my fold with system fold" <| fun (list: list<int>) ->
            if not list.IsEmpty
            then
                Expect.equal (MyList.Fold (+) 0 (TransformToMyList list))  (List.fold (+) 0 list)
            else
                Expect.sequenceEqual list []
         testProperty "Comparision my fold with system fold, division" <| fun (list: list<int>) ->
            let lst = List.filter (fun x -> x <> 0) list
            if not lst.IsEmpty
            then
                let expected = List.fold (fun x y -> x / y) lst.Head lst.Tail
                let res =
                    match TransformToMyList lst with
                    | Cons (head, tail) -> MyList.Fold (fun x y -> x / y) head tail
                    | Single x -> x
                Expect.equal res expected
            else
                Expect.sequenceEqual lst []
         testProperty "Comparision my getLength with system getLength" <| fun (list: list<int>) ->
            if not list.IsEmpty
            then
                Expect.equal (MyList.Length (TransformToMyList list)) (list.Length)
            else
                Expect.sequenceEqual list []
         testProperty "Comparision my map with system map" <| fun (list: list<int>) ->
             if not list.IsEmpty
             then
                 Expect.equal (TransformToSystemList (MyList.Map (fun x -> x*x) (TransformToMyList list)))  (List.map (fun x -> x*x) list)
             else
                 Expect.sequenceEqual list []       
         testProperty  "Comparision my concat with system concat" <| fun (list1: list<int>, list2: list<int>) ->
             if not list1.IsEmpty && not list2.IsEmpty
             then
                 Expect.sequenceEqual (TransformToSystemList (MyList.Concat (TransformToMyList list1) (TransformToMyList list2)))   (list1 @ list2)
             else
                 if list1.IsEmpty
                 then Expect.equal list1 []
                 else Expect.equal list2 []
        ]

[<Tests>]
let TestsForStrings =
    testList "MyString"
        [testCase "Comparision my concat for string with system concat " <| fun _ ->
            let string1 = "Hello, "
            let string2 = "world"
            let expected = TransformToMyString (string1 + string2)
            let res = MyString.Concat  (TransformToMyString string1)  (TransformToMyString string2)
            Expect.equal res expected ""
        ]

[<Tests>]
let TestsForTrees =
    testList "MyString"
        [testCase "Test for maximum of tree" <| fun _ ->
            let tree = Node (10, TransformToMyList ([Leaf(9); Leaf(-5); Leaf(1)]))
            let res = getMaximum tree
            Expect.equal res 10 ""
         testCase "Test for average of tree" <| fun _ ->
            let tree = Node (6, TransformToMyList ([Leaf(11); Leaf(-5); Leaf(8)]))
            let res = getAverage tree
            Expect.equal res 5.0 ""
        ]
