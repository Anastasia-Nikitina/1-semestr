module Tests


  open Expecto
  open Program

  [<Tests>]
   let tests =
      testList "Homework_2 Tests"
        [testCase "Если x=0, результат равен нулю" <| fun _ ->
          let subject = Functions.first 0
          Expect.equal subject 1 "0 в любой степени дает 0 "

         testCase "Если х=-1, результат равен 1" <| fun _ ->
          let subject = Functions.first -1
          Expect.equal subject 1 "-1 в четной степени дает 1, а в нечетной:-1"

         testCase "Если x=0, результат равен 0" <| fun _ ->
          let subject = Functions.second 0
          Expect.equal subject 1 "0 в любой степени(кроме нулевой) дает 0. "
     
         testCase "Если х=-1, результат равен один" <| fun _ ->
          let subject = Functions.second -1
          Expect.equal subject 1 "-1 в четной степени дает 1, в нечетной -1"

         testCase "Любое отрицательное число всегда меньше нуля" <| fun _ ->
           let subject = Functions.third  ([|5; 7; 9; -1|], 0)
           Expect.equal subject [|3|] "-1-отрицательное число, значит -1<0"

         testCase "Любое положительное число всегда меньше отрицательного" <| fun _ ->
           let subject = Functions.third ([|-5; -12; -3; -8|], 10)
           Expect.equal subject [|0; 1; 2; 3|] "Так как 10-положительное число, оно больше всех отрицательных"

         testCase "Ноль находится в промежутке между -1 и 1" <| fun _ ->
             let subject = Functions.fourth ([|16; 31; 0; -4; 0; 11|], -1, 1)
             Expect.equal subject [|0; 1; 3; 5|] "Результат должен выводить массив с номерами элементов-нулей"

         testCase "Нулевой и первый элементы массива должны меняться местами" <| fun _ ->
             let subject = Functions. fifth [|8; 10|]
             Expect.equal subject [|10; 8|] "Нулевой и первый элементы массива должны меняться местами"

         testCase "Нулевой и пооследний элементы массива должны меняться местами" <| fun _ ->
             let subject = Functions.sixth ([|1; 2; 3; 4; 5|], 0, 4)
             Expect.equal subject [|5; 2; 3; 4; 1|] "1(нулевой элемент) и 5(последний элемент)
             должны поменяться местами"
         ]     
