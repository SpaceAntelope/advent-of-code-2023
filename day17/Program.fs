// namespace Day17


// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"
open Day17
open Common

"./Input/puzzle.example"
|> Part1.parse
|> Part1.pathFinding
|> List.min
|> Assertions.shouldBe 102