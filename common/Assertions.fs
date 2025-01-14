namespace Common

    module Assertions =

        let shouldBe expected actual= 
            if expected <> actual 
            then 
                failwithf "Comparison between expected %A and actual %A failed." expected actual
        let shouldBeAndTee expected actual= 
            if expected <> actual 
            then 
                failwithf "Comparison between expected %A and actual %A failed." expected actual
            else actual

        let seqShouldBe expected actual= 
            if Seq.length expected <> Seq.length actual then failwithf $"Seq comparison failed because element count doesn't match. Expected count is {Seq.length expected} while actual count is {Seq.length actual}"

            Seq.zip expected actual
            |> Seq.iteri (fun index (expected, actual) -> 
                    if expected <> actual 
                    then failwithf "Comparison between expected %A and actual %A failed at index %d." expected actual index)

