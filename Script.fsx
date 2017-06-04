let sprintArray (arr : List<int>) = 
    let error = ""
    arr |> List.fold (fun acc elem -> 
               match acc with
               | "" -> elem.ToString()
               | _ -> acc + "," + elem.ToString()) error

let validIndex (dIndex:string*int)=
    let d,index=dIndex
    index<>(-1)
let indexKey dIndex=
    let d,index=dIndex
    index
    
let rec split (text : string)  (d:List<string>)  = 
    let myNumbers = 
        match text with
        | "" -> [""]
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
                match newLineIndex with
                |(-1)->failwith("no new line after delimiter")
                |_->text.Substring(2,newLineIndex-2) |>getdelimiterList
            else
                [",";"\n"]
    ("\n"::delimiterList,text.Substring(newLineIndex+1))


let calculate (text : string)  = 
    let d,numberOfString=getdelimitersWithText  text
    let numbers = 
        match text with
        | "" -> [ 0 ]
        | _ -> 
            let integers = split numberOfString d |> List.map System.Int32.Parse |> List.filter (fun n -> n <= 1000)
            let negative = List.filter (fun n -> n < 0) integers
            match negative with
            | [] -> integers 
            | _ -> failwith (sprintf "negative not allowed %s" (sprintArray negative))
    List.sum numbers

    
let t1 = "1,2"
let t2 = "1,2\n3"
let t3 = "//[;][**]\n1;2**3"
let t4="//[,,,][;]\n1,,,2;3,,,4"
let t5="//[;]\n1;2;3;4"
let t6="//[;]\n1;2;3;4"
let t7="//[;]\n1;2;3;1001"
let t8="//[;]\n1;-1;3;4"
let t9="//[;]\n123;2;1;"
calculate ""
calculate t1
calculate t2
calculate t3
calculate t4
calculate t5
calculate t6
calculate t7
calculate t8
calculate t9
