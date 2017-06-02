let rec addIntegers numbers=
   match numbers with
   |[]->0
   |[first]->first
   |first::rest-> first+addIntegers rest
let sprintArray (arr:int[])=
    let error=""
    arr|>Array.fold(fun acc elem-> match acc with |""->elem.ToString() |_->acc+elem.ToString()) error
let addTwoNumbers (text:string) d=
   let numbers= match text with
    |""->[0]
    |_->
        let integers=text.Split[|d;'\n'|]|>Array.map System.Int32.Parse 
        let negative=Array.filter(fun n-> n<0) integers
        match negative with
        |[||]->negative|>Array.toList
        |_-> failwith(sprintf "negative not allowed %s" (sprintArray negative))
   addIntegers numbers

let first (text:string)=
   let d,text'= match text with
        |x when text.StartsWith("//")->(text.Chars(2),text.Substring(4))
        |_->(';',text)
   addTwoNumbers text' d

let text="1;2;3;4"
let n=[|1,2,3,4,5|]
first text