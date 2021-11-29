module functionsHw4

open System.IO

let readArray file =
    let a = File.ReadAllLines file
    let intArr = Array.zeroCreate a.Length
    let mutable j = 0
    for i in a do
        intArr.[j] <- int (i.Trim())
        j <- j + 1
    intArr

let readList file =
    let a = File.ReadAllLines file
    let intList = [ for i in a -> int (i.Trim()) ]
    intList

let arrayIntoString (array: array<int>) =
    let stringArr: array<string> = Array.zeroCreate array.Length
    for i = 0 to array.Length - 1 do
        stringArr.[i] <- string array.[i]
    stringArr

let writeArray file array =
    File.WriteAllLines(file, arrayIntoString array)

let writeList file list =
    File.WriteAllLines(file, arrayIntoString (List.toArray list))

let bubbleSortOfArray (arr: array<_>) =
    for i = 1 to arr.Length - 1 do
        for j = 0 to arr.Length - 2 do
            if arr.[j] > arr.[j + 1]
            then
                let x = arr.[j + 1]
                arr.[j + 1] <- arr.[j]
                arr.[j] <- x
    arr

let quickSortOfList list =
    let rec sort list1 =
        match list1 with
        | [] -> []
        | pivot :: tail ->
            let less = List.filter ((>) pivot) tail
            let great = List.filter ((<=) pivot) tail
            sort less @ [ pivot ] @ sort great
    sort list

let bubbleSortOfList list =
    let rec bubble list1 =
        match list1 with
        | [] -> []
        | head :: [] -> [ head ]
        | head1 :: head2 :: tail ->
            if head1 > head2
            then head2 :: bubble (head1 :: tail)
            else head1 :: bubble (head2 :: tail)
    let rec go (list2: list<_>) k =
        if k <> list2.Length
        then go (bubble list2) (k + 1)
        else list2
    go list 0

let quickSortOfArray a =
    let rec sort (arr: array<_>, first, last) =
        let mutable i = first
        let mutable j = last
        let pivot = arr.[(first + last) / 2]
        while i <= j do
            while arr.[i] < pivot do
                i <- i + 1
            while arr.[j] > pivot do
                j <- j - 1
            if i <= j
            then
                let x = arr.[i]
                arr.[i] <- arr.[j]
                arr.[j] <- x
                i <- i + 1
                j <- j - 1
        if i < last
        then
            sort (arr, i, last)
        if first < j
        then
            sort (arr, first, j)
    if a = [||]
    then a
    else
        sort (a, 0, a.Length - 1)
        a

let packing32BitNumber (x: int32, y: int32) =
    if y >= 0
    then (int64 x <<< 32) + int64 y
    else (int64 (x + 1) <<< 32) + int64 y

let unPacking64to32BitNumber (a: int64) =
    int32 (a >>> 32), int32 ((a <<< 32) >>> 32)

let packing16BitNumber (a: int16, b: int16, c: int16, d: int16) =
    let pack16to32 (x: int16, y: int16) =
        if y >= 0s
        then (int32 x <<< 16) + int32 y
        else (int32 (x + 1s) <<< 16) + int32 y
    packing32BitNumber (pack16to32 (a, b), pack16to32 (c, d))

let unPacking64to16BitNumber (x: int64) =
    let unPack32to16 (a: int32) =
        int16 (a >>> 16), int16 ((a <<< 16) >>> 16)
    let a, b = unPacking64to32BitNumber x
    unPack32to16 a, unPack32to16 b
