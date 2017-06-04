let rec addIntegers numbers = 
    match numbers with
    | [] -> 0
    | [ first ] -> first
    | first :: rest -> first + addIntegers rest

let sprintArray (arr : int []) = 
    let error = ""
    arr |> Array.fold (fun acc elem -> 
               match acc with
               | "" -> elem.ToString()
               | _ -> acc + "," + elem.ToString()) error

let addTwoNumbers (text : string) d = 
    let numbers = 
        match text with
        | "" -> [ 0 ]
        | _ -> 
            let integers = 
                text.Split [| d; '\n' |]
                |> Array.filter (fun s -> 
                       match s with
                       | "" -> false
                       | _ -> true)
                |> Array.map System.Int32.Parse
                |> Array.filter (fun n -> n <= 1000)
            
            let negative = Array.filter (fun n -> n < 0) integers
            match negative with
            | [||] -> integers |> Array.toList
            | _ -> failwith (sprintf "negative not allowed %s" (sprintArray negative))
    addIntegers numbers

let first (text : string) = 
    let d, text' = 
        match text with
        | x when text.StartsWith("//") -> 
            match text.Chars(2) with
            | '[' -> 
                let index = text.IndexOf(']')
                (text.Substring(3, index - 3), text.Substring(index + 1))
            | _ -> failwith ("invalid pattern")
        | _ -> (";", text)
    d

let validIndex (dIndex:string*int)=
    let d,index=dIndex
    index<>(-1)
let indexKey dIndex=
    let d,index=dIndex
    index
    
let rec split (text : string)  (d:List<string>)  = 
    let myNumbers = 
        match text with
        | "" -> []
        | _ -> 
            let indexies= List.map(fun (x:string)-> x,text.IndexOf(x)) d|>List.filter validIndex|>List.sortBy indexKey
            match indexies with 
            |[]->[text]
            |(delimiter,index)::rest->
                let number = text.Substring(0,index)
                [number]@ split (text.Substring(index+delimiter.Length))  d
    myNumbers

let rec getdelimiterList (text:string) =
    match text with
        |""->[]
        | text when text.StartsWith("[")->
            let index=text.IndexOf(']')
            let d=text.Substring(1,index-1)
            [d]@getdelimiterList (text.Substring(index+1))
        |_->failwith("invalid dilemeter")

let getdelimitersWithText (text:string)=
    let newLineIndex=text.IndexOf("\n")
    let delimiterList=
        if text.StartsWith("//") then
           text.Substring(2,newLineIndex-2) |>getdelimiterList
        else
            [","]
    (delimiterList,text.Substring(newLineIndex+1))


    
let t1 = "1,2"
let t2 = "1,2\n3"
let t3 = "//[;][**]\n1;2**3"
let t4="1,,,2;3,,,4"
let t5="1,2;3;4"
t3.Substring(2,3)
let d,text=getdelimitersWithText  t3
let numbers=split text d |>List.map System.Int32.Parse|>List.sum
