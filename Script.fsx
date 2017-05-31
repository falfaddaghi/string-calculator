let addTwoNumbers (text:string)=
   let numbers= match text with
    |""->[0]
    |_->text.Split[|','|] |>Array.map System.Int32.Parse |>Array.toList
   match numbers with
   |[first]->first
   |[first;second]->first+second


   

