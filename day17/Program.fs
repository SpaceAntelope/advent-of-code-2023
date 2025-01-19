// namespace Day17


// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"
open Day17
open Common

"./Input/puzzle.example"
|> Part1.parse
|> Part1.DykeBuilder
|> Assertions.shouldBe 102

"./Input/puzzle.input"
|> Part1.parse
|> Part1.DykeBuilder
|> printfn "The least heat loss the crucible can incur is %d"