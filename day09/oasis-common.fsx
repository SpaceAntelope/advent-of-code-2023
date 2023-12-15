open System

let parse (data: string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map _.Split(' ')
    |> Array.map (Array.map Convert.ToInt32)


let rec diff (line: int array) =
    line
    |> Array.pairwise
    |> Array.map (fun (x, y) -> y - x)
    |> function
        | x when Array.forall (fun x -> x = 0) x -> [| x |]
        | x -> [| yield x; yield! (diff x) |]

let sumExtrapolatedNext (table: int[][]) =
    table
    |> Array.rev
    |> Array.fold (fun sum layer -> (layer |> Array.last) + sum) 0

let sumExtrapolatedPrevious (table: int[][]) =
    table
    |> Array.rev
    |> Array.fold (fun sum layer -> (layer |> Array.head) - sum) 0