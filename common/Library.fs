namespace Common

module General =
    open System

    let tee msg x = 
        printfn "%s: %A" msg x
        x

    let teeConditional (f: unit -> bool) msg x = 
        if f() then printfn "%s: %A" msg x
        x

    let manhattan (a: int*int) (b: int*int) = 
        let r1,c1 = a
        let r2,c2 = b
        Math.Abs(r2-r1) + Math.Abs(c2-c1)

    let nSimple (r,c) =
        [r+1,c;r-1,c;r,c+1;r,c-1]