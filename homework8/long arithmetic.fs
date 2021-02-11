module long_arithmetic
open MyList
open functionsHw4

[<Struct>]
type NumberWithSign =
    val sign: bool
    val number: MyList<int>
    new(a, s) = {number = a; sign = s}

let TransformStringToMyList (string: string) =
    let list =
        [for i =  string.Length - 1  downto 0 do // строка считывается сразу перевернутая
            int string.[i] - 48
        ]
    if int string.[0] = 45
    then NumberWithSign(TransformToMyList list, true)
    else NumberWithSign(TransformToMyList list, false)

let TransformStringToMyList1 (string: string) =
    let list =
        [for i =0 to  string.Length - 1   do // строка не переворачивается
            int string.[i] - 48
        ]
    TransformToMyList list

let comparision (s1: string, s2: string) =
    let mutable k = 0
    if s1.Length > s2.Length
    then k <- 1
    elif s1.Length < s2.Length
    then k <- 2
    else
        let mutable i = s1.Length
        while (k = 0) && (i >= 0)  do
            if s1.[i] > s2.[i]
            then k <- 1
            elif s1.[i] < s2.[i]
            then k <- 2
            i <- i - 1
    k


let addZeros (x: NumberWithSign, y: NumberWithSign) =   
   let mutable a = x
   let mutable b = y
   let dif = abs (Length x.number - Length y.number)
   if dif <> 0 then
       let lst =
           [ for j = 0 to dif - 1 do
               0
           ]
       let zero = TransformToMyList lst // дополняем нулями, чтобы числа были одинаковой длины
       if Length x.number <= Length y.number
       then            
           a <- NumberWithSign(Concat x.number zero, x.sign)
       else            
           b <- NumberWithSign(Concat y.number zero, y.sign)
   (a, b)

let flipRemoveZeros (f, a: NumberWithSign, b: NumberWithSign) =
    let perevertysh = 
        arrayIntoString (List.toArray (TransformToSystemList (f (a.number, b.number))))
    let mutable (res: string) = perevertysh.[perevertysh.Length - 1]
    for i = perevertysh.Length - 2 downto 0 do // переворачиваем обратно
        res <- res + perevertysh.[i]       
    let finish = res.TrimStart('0') // убираем нули в начале
    finish


    
let addition (x: NumberWithSign, y: NumberWithSign) =
    
    let (a, b) = addZeros (x, y)
    let mutable transfer = 0
    let c = Length a.number - 1
    let rec go (a: MyList<_>, b: MyList<_>)  =       
       match (a, b) with
       | (Cons(head_a, tail_a), Cons(head_b, tail_b)) ->                   
           let h = (head_a + head_b + transfer) % 10 
           transfer <- (head_a + head_b + transfer) / 10 // перенос через разряд
           Cons(h , go (tail_a, tail_b))
       | (Single a, Single b) ->         
           Cons((a + b + transfer) % 10 , Single ((a + b + transfer) / 10))
    flipRemoveZeros (go, a, b)
   

   (* if x.sign = y.sign
    then
        if x.sign = true
        then "-" + flipRemoveZeros (go, a.number, b.number)
        else flipRemoveZeros (go, a.number, b.number) *)
    
      
    

let substraction (x: NumberWithSign, y: NumberWithSign) =
    let (a, b) = addZeros (x, y)
    let mutable transfer = 0
    let rec go (a: MyList<_>, b: MyList<_>) =
        match (a, b) with
        | (Cons(head_a, tail_a), Cons(head_b, tail_b)) ->
            if head_a - transfer < head_b
            then
                let h = head_a + 10 - head_b - transfer
                transfer <- 1
                Cons(h, go (tail_a, tail_b))
            else
                let h = head_a - head_b - transfer
                transfer <- 0
                Cons(h, go (tail_a, tail_b)) 
        | (Single a, Single b) ->
            Single (a - transfer - b)
            
    flipRemoveZeros (go, a, b)


    
