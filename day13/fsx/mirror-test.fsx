#load "./mirror-common.fsx"

open System
open ``Mirror-common``

let horizontalSymmetryData = @"
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."

let verticalSymmetryData = @"
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"


let sliceIndexTest = 
    let indexes r1 r2 ln = 
        let x = [0..ln-1]
        printfn $"left: {r1} right: {ln-r2} r1>ln-r2 -> {r1>ln-r2}"
        printfn $"%A{x[0..r1]},%A{x[r2..r2+r1]}"
        printfn $"%A{x[r1-(ln-r2-1)..r1]},%A{x[r2..]}"
        printfn $"%A{x[..r1]},%A{x[r2..]}"
    
    indexes 5 6 8
    indexes 5 6 15
    indexes 5 6 11
    
let testSumSymScore (data: string[]) = 
    let expected = 405
    data     
    |> Array.map parse
    |> Array.sumBy symScore
    |> function
    | actual when expected = actual -> "Sym score sum test passed"
    | actual -> $"Sym score sum test failed: expected {expected} but got {actual}"
    |> printfn "%s"

testSumSymScore [| horizontalSymmetryData; verticalSymmetryData|]