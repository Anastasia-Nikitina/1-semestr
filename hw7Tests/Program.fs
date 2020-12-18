module hw7Tests
open MyList
open MyString
open MyTree
open Expecto
[<Tests>]
let TestsForLists =
    testList "MyList"
        [testCase "Comparision my fold with system fold " <| fun _ ->
            let list = [1; 2; 3; 4; 5]
            let res = MyList.fold (fun sum x -> x + sum) 0 (TransformToMyList list)
            let expected = List.fold (fun sum x -> x + sum) 0 list
            Expect.equal res expected ""
         testCase "Comparision my concat for list with system concat" <| fun _ ->
            let list1 = [0; 1; 2; 3; 4]
            let list2 = [5; 6; 7; 8; 9]
            let expected = TransformToMyList  (list1 @ list2)
            let res = MyList.Concat (TransformToMyList list1) (TransformToMyList list2)
            Expect.equal res expected ""
         testCase "Comparision my getLength with system getLength" <| fun _ ->
            let list = [1; 2; 3; 4; 5]
            let res = MyList.Length (TransformToMyList list)
            let expected = list.Length
            Expect.equal res expected ""
         testCase "Comparision my sort with system sort" <| fun _ ->
            let list = [5; 4; 6; 10; 1]
            let res = MyList.sort (TransformToMyList list)
            let expected = TransformToMyList (List.sort list)
            Expect.equal res expected ""
         testCase "Comparision my map with system map" <| fun _ ->
            let list = [1; 2; 3; 4; 5]
            let res = MyList.Map (fun x -> x*x)  (TransformToMyList list)
            let expected = TransformToMyList (List.map (fun x -> x*x) list)
            Expect.equal res expected ""    
         
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
