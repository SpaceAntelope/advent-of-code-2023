// namespace Day17


// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"
open Day17
open Common
open System.IO

let parse path =
    path
    |> File.ReadAllLines
    |> Array.map (fun row ->
        row.ToCharArray()
        |> Array.map (fun c -> int (c - '0')))
    |> array2D


"./Input/puzzle.example"
|> parse
|> Part1.DykeBuilder
|> Assertions.shouldBe 102

"./Input/puzzle.input"
|> parse
|> Part1.DykeBuilder
|> printfn "The least heat loss the crucible can incur is %d"

"./Input/puzzle.example"
|> parse
|> Part2.DykeBuilder
|> Assertions.shouldBe 94

"./Input/puzzle.example2"
|> parse
|> Part2.DykeBuilder
|> Assertions.shouldBe 71

"./Input/puzzle.input"
|> parse
|> Part2.DykeBuilder
|> printfn "The least heat loss the ultra crucible can incur is %d"