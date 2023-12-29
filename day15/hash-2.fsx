#load "./hash-common.fsx"

open ``Hash-common``

open System
open System.IO

let boxes = initBoxes()

"./puzzle.input"
|> File.ReadAllText
|> _.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
|> Array.iter (updateBoxes boxes)

boxes 
|> focusingPower
|> printfn "Focusing power calculated at %d"