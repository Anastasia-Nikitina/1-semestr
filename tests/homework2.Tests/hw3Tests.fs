module hw3Tests


open Expecto
open Program
 [<Tests>]
 let PropertyTests =
   testList "Property tests"
    [testProperty "Result of task 2 = result of task 3" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib2 (abs n)) (FunctionsHw3.fib3 (abs n)) "Result of task 2 must be equal result of task 3"
     testProperty "Result of task 3 = result of task 4" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib3 (abs n)) (FunctionsHw3.fib4 (abs n)) "Result of task 3 must be equal result of task 4"
     testProperty "Result of task 2 = result of task 4" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib2 (abs n)) (FunctionsHw3.fib4 (abs n)) "Result of task 2 must be equal result of task 4"
     testProperty "Result of task 4 = result of task 5" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib4 (abs n)) (FunctionsHw3.fib5 (abs n)) "Result of task 4 must be equal result of task 5"
     testProperty "Result of task 2 = result of task 5" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib2 (abs n)) (FunctionsHw3.fib5 (abs n)) "Result of task 2 must be equal result of task 5"
     testProperty "Result of task 3 = result of task 5" <| fun (n: int) ->
         Expect.equal (FunctionsHw3.fib3 (abs n)) (FunctionsHw3.fib5 (abs n)) "Result of task 3 must be equal result of task 5"
    ]

 [<Tests>]
 let fib1Tests =
   testList "First task tests"
    [testCase "The zero Fibonacci number using recursion" <| fun _ ->
         let subject = FunctionsHw3.fib1 0
         Expect.equal subject 0 "The zero Fibonacci number is 0"
     testCase "The first Fibonacci number using recursion" <| fun _ ->
         let subject = FunctionsHw3.fib1 1
         Expect.equal subject 1 "The first Fibonacci number is 1"
     testCase "The sixth Fibonacci number using recursion" <| fun _ ->
         let subject = FunctionsHw3.fib1 6
         Expect.equal subject 8 "The first Fibonacci number is 0"
    ]

 [<Tests>]
 let fib2Tests =
   testList "Second task tests"
    [testCase "The zero Fibonacci number using iterative method" <| fun _ ->
          let subject = FunctionsHw3.fib2 0
          Expect.equal subject 0 "The zero Fibonacci number is 0"
     testCase "The first Fibonacci number using iterative method" <| fun _ ->
          let subject = FunctionsHw3.fib2 1
          Expect.equal subject 1 "The first Fibonacci number is 1"
     testCase "The fifth Fibonacci number using iterative method" <| fun _ ->
          let subject = FunctionsHw3.fib2 5
          Expect.equal subject 5 "The fifth Fibonacci number is 5"
     ]

 [<Tests>]
 let fib3Tests =
    testList "Third task tests"
     [testCase "The zero Fibonacci number using tail recursion" <| fun _ ->
           let subject = FunctionsHw3.fib3 0
           Expect.equal subject 0 "The zero Fibonacci number is 0"
      testCase "The first Fibonacci number using tail recursion" <| fun _ ->
           let subject = FunctionsHw3.fib3 1
           Expect.equal subject 1 "The first Fibonacci number is 1"
      testCase "The sixth Fibonacci number using tail recursion" <| fun _ ->
           let subject = FunctionsHw3.fib3 6
           Expect.equal subject 8 "The first Fibonacci number is 0"
      testCase "The tehth Fibonacci number using tail recursion" <| fun _ ->
           let subject = FunctionsHw3.fib3 10
           Expect.equal subject 55 "The tenth Fibonacci number is 55" 
       ]

 [<Tests>]
 let fib4Tests =
     testList "Fourth task tests"
      [testCase "The zero Fibonacci number using tail recursion" <| fun _ ->
          let subject = FunctionsHw3.fib4 0
          Expect.equal subject 0 "The zero Fibonacci number is 0"
       testCase "The first Fibonacci number using naive matrix method" <| fun _ ->
          let subject = FunctionsHw3.fib4 1
          Expect.equal subject 1 "The first Fibonacci number is 1"
       testCase "The seventh Fibonacci number using naive matrix method" <| fun _ ->
          let subject = FunctionsHw3.fib4 7
          Expect.equal subject 13 "The seventh Fibonacci number is 13"
       testCase "The nineth Fibonacci number using naive matrix method" <| fun _ ->
          let subject = FunctionsHw3.fib4 9
          Expect.equal subject 34 "The nineth Fibonacci number is 34" 
      ]
 [<Tests>]
 let fib5Tests =
    testList "Fifth task tests"
     [testCase "The zero Fibonacci number using matrix for logarithm" <| fun _ ->
         let subject = FunctionsHw3.fib5 0
         Expect.equal subject 0 "The zero Fibonacci number is 0"
      testCase "The first Fibonacci number using matrix for logarithm" <| fun _ ->
         let subject = FunctionsHw3.fib5 1
         Expect.equal subject 1 "The first Fibonacci number is 1"
      testCase "The seventh Fibonacci number using matrix for logarithm" <| fun _ ->
         let subject = FunctionsHw3.fib5 7
         Expect.equal subject 13 "The seventh Fibonacci number is 13"
      testCase "The nineth Fibonacci number using matrix for logarithm" <| fun _ ->
         let subject = FunctionsHw3.fib5 9
         Expect.equal subject 34 "The nineth Fibonacci number is 34" 
     ]

 [<Tests>]
   let fib6Tests =
     testList "Sixth task tests"
      [testCase "Fibonacci numbers to 0" <| fun _ ->
          let subject = FunctionsHw3.fib6 0
          Expect.equal subject [|0|] "The zero Fibonacci number is 0"
       testCase "Fibonacci numbers to 1" <| fun _ ->
          let subject = FunctionsHw3.fib6 1
          Expect.equal subject [|0; 1|] "The Fibonacci numbers to 1 are 0, 1"
       testCase "Fibonacci numbers to 5" <| fun _ ->
          let subject = FunctionsHw3.fib6 5
          Expect.equal subject [|0; 1; 1; 2; 3; 5|] "The Fibonacci numbers to five are 0; 1; 1; 2; 3; 5 "
       testCase "Fibonacci numbers to 8" <| fun _ ->
           let subject = FunctionsHw3.fib6 8
           Expect.equal subject [|0; 1; 1; 2; 3; 5; 8; 13; 21|] "The Fibonacci numbers to eight are 0; 1; 1; 2; 3; 5; 8; 13; 21"
      ]

