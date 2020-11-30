module hw6Tests
open Expecto
open hw6

[<Tests>]
let hw6Tests =
    testList "Multiplication matrices"
        [testCase "Multiplication 2x2 matrices" <| fun _ ->
            let matrix1 = new BoolMatrix (2, 2, [Coordinates(0<_line>, 0<_colomn>); Coordinates(1<_line>, 1<_colomn>)])
            let matrix2 = new BoolMatrix (2, 2, [Coordinates(0<_line>, 1<_colomn>); Coordinates(1<_line>, 0<_colomn>)])
            let res = (multBoolMatrix (matrix1, matrix2)).Coordinate
            let expected = [Coordinates(0<_line>, 1<_colomn>); Coordinates(1<_line>, 0<_colomn>)]
            Expect.equal res expected ""
         testCase "Multiplication 3x3 matrices" <| fun _ ->
            let matrix1 = new BoolMatrix (3, 3, [Coordinates(0<_line>, 0<_colomn>); Coordinates(1<_line>, 2<_colomn>)])
            let matrix2 = new BoolMatrix (3, 3, [Coordinates(1<_line>, 1<_colomn>); Coordinates(2<_line>, 1<_colomn>)])
            let res = (multBoolMatrix (matrix1, matrix2)).Coordinate
            let expected = [Coordinates(1<_line>, 1<_colomn>)]
            Expect.equal res expected ""
         testCase "Multiplication empty matrices" <|fun _ ->
            let matrix1 = new BoolMatrix (3, 3, [])
            let matrix2 = new BoolMatrix (3, 3, [])
            let res = (multBoolMatrix (matrix1, matrix2)).Coordinate
            let expected = []
            Expect.equal res expected ""
         testCase "Multiplication 3x2 and 2x3 matrices" <| fun _ ->
            let matrix1 = new BoolMatrix (3, 2, [Coordinates(0<_line>, 0<_colomn>); Coordinates(1<_line>, 1<_colomn>)])
            let matrix2 = new BoolMatrix (2, 3, [Coordinates(0<_line>, 2<_colomn>); Coordinates(1<_line>, 1<_colomn>)])
            let res = (multBoolMatrix (matrix1, matrix2)).Coordinate
            let expected = [Coordinates(0<_line>, 2<_colomn>); Coordinates(1<_line>, 1<_colomn>)]
            Expect.equal res expected  ""
        ]   
