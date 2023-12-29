#load "./hash-common.fsx"

open ``Hash-common``

open System
open System.Collections.Generic

let testCases = 
    [   "HASH", 52
        "rn=1", 30;
        "cm-", 253;
        "qp=3", 97;
        "cm=2", 47;
        "qp-", 14;
        "pc=4", 180;
        "ot=9", 9;
        "ab=5", 197;
        "pc-", 48;
        "pc=6", 214;
        "ot=7", 231; ]

let testHash = 
    testCases
    |> List.iter (fun (source,expected) ->
        match hash source with
        | actual when actual = expected -> printfn "Succesfully hashed %A" source
        | actual -> failwith $"Hash test failed for {source}. Expected {expected} but got {actual}.")    

let testResultSum  =
    let expected = 1320
    testCases
    |> List.skip 1
    |> List.map fst
    |> List.sumBy hash
    |> function
    | actual when actual = expected -> printfn "Passed sum hash test."
    | actual -> failwith $"Failed sum hash test. Expected {expected} but got {actual}."

let testUpdateBoxes = 
    let expected = Array.init 256 (fun idx -> 
        match idx with 
        | 0 -> ResizeArray([lens "rn" 1; lens "cm" 2])
        | 3 -> ResizeArray([lens "ot" 7; lens "ab" 5; lens "pc" 6])
        | _ -> ResizeArray<Lens>())
    
    let actual = initBoxes()
    testCases
    |> List.skip 1
    |> List.map fst
    |> List.iter (updateBoxes actual)

    let expectedStr = boxes2str expected
    let actualStr = boxes2str actual

    if expectedStr = actualStr then printfn "Update boxes test passed."
    else failwith $"Update boxes test failed: Expected {expectedStr} but got {actualStr}"


let testFocusingPower =
    let expected = 145
    let boxes = initBoxes()
    
    testCases
    |> List.skip 1
    |> List.map fst
    |> List.iter (updateBoxes boxes)

    match focusingPower boxes with
    | actual when actual = expected -> printfn "Focusing power test passed."
    | actual  -> printfn $"Focusing power test failed. Expected {expected} but got {actual}."