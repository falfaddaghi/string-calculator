let rec addIntegers numbers=
   match numbers with
   |[]->0
   |[first]->first
   |first::rest-> first+addIntegers rest

let addTwoNumbers (text:string)=
   let numbers= match text with
    |""->[0]
    |_->text.Split[|','|] |>Array.map System.Int32.Parse |>Array.toList
   addIntegers numbers


   

