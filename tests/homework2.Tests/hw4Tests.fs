module hw4Tests
open Expecto
open ProgramHw4
open functionsHw4


[<Tests>]

let ComparisionWithSystemSort =
    testList "Equal with system sort"
      [testProperty  "Bubble sort array" <| fun(arr:array<int>) ->
        Expect.sequenceEqual (bubbleSortOfArray arr) (Array.sort arr) "Result of bubble sort for array must be equal to system sort"
       testProperty  "Bubble sort list" <| fun(list:list<int>) ->
        Expect.sequenceEqual (bubbleSortOfList list) (List.sort list) "Result of bubble sort for list must be equal to system sort"
       testProperty  "Quick sort list" <| fun(list:list<int>) ->
        Expect.sequenceEqual (quickSortOfList list) (List.sort list) "Result of quick sort for list must be equal to system sort"
       testProperty  "Quick sort array" <| fun(arr:array<int>) ->
        Expect.sequenceEqual (quickSortOfArray arr) (Array.sort arr) "Result of quick sort for array must be equal to system sort"     
      ]
[<Tests>]
let ComparisionMySorts =
    testList "Equal my sorts"
      [testProperty  "Bubble sort array and quick sort array" <| fun(arr:array<int>) ->
        Expect.sequenceEqual (bubbleSortOfArray arr) (bubbleSortOfArray arr) "Result of bubble sort for array must be equal to quick sort"
       testProperty  "Bubble sort list and quick sort list" <| fun(list:list<int>) ->
        Expect.sequenceEqual (quickSortOfList list) (bubbleSortOfList list) "Result of bubble sort for list must be equal to quick sort"
      ]
[<Tests>]
let ComparisionPackingWithUnpacking =
    testList "Packing back unpacking"
     [testProperty "Packing 32 to 64 and unpacking back" <| fun(a: int32, b:int32) ->
        Expect.equal  (unPacking64to32BitNumber(packing32BitNumber (a, b))) (a, b) "Results must be equal"
      testProperty "Packing 16 to 64 and unpacking back" <| fun(a: int16, b:int16, c:int16, d: int16) ->
        Expect.equal  (unPacking64to16BitNumber(packing16BitNumber (a, b, c, d))) ((a, b), (c, d)) "Results must be equal"  
     ]

     
      
    




