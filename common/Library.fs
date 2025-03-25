namespace Common

module General =
    open System
    open System.Text.RegularExpressions

    let tee msg x = 
        printfn "%s: %A" msg x
        x
    let tee2 msg x = 
        sprintf "%A" x |> fun str -> Regex.Replace(str, "\\s+", " ").Replace(";",";\n")
        |> printfn "%s:\n%s" msg
        x


    let teeConditional (f: unit -> bool) msg x = 
        if f() then printfn "%s: %A" msg x
        x

    let manhattan (a: int*int) (b: int*int) = 
        let r1,c1 = a
        let r2,c2 = b
        Math.Abs(r2-r1) + Math.Abs(c2-c1)
    
    let manhattan64 ((a: int64*int64), (b: int64*int64)) = 
        let r1,c1 = a
        let r2,c2 = b
        Math.Abs(r2-r1) + Math.Abs(c2-c1)

    let nSimple (r,c) =
        [r+1,c;r-1,c;r,c+1;r,c-1]