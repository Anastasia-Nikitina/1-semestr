module MyString
open MyList

type MyString = MyList<char>

let Concat (string1: MyString) (string2: MyString) =
    MyList.Concat string1 string2

let TransformToMyString (string: string) =
    let list =
        [for i = 0 to string.Length - 1  do
            string.[i]
        ]
    let res: MyString = TransformToMyList list
    res

