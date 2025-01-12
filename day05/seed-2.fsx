open System
open System.IO
open System.Diagnostics

#load "../Global.fsx"

type Range = { Start: int64; Stop: int64 } 
    with member x.Contains(n: int64) = x.Start <= n && x.Stop >= n
type MapEntry = { Dst : Range; Src: Range; Length: int64 }
type MapIndex = { From: string; To: string; Entries: MapEntry array }
type SeedRange = { From: int64; Length: int64 } 
    with 
        member x.Seeds = 
            seq { for seed in x.From..x.From + x.Length - 1L do yield seed }

let parse path =
    let text = path |> File.ReadAllText
    let seedRanges = 
        text.Split(Environment.NewLine)
        |> Array.head
        |> _.Trim().Split(' ') 
        |> Array.skip 1
        |> Array.map int64
        |> Array.chunkBySize 2
        |> Array.map (fun arr -> { From = arr.[0]; Length = arr.[1] })

    let maps = 
        text.Split($"{Environment.NewLine}{Environment.NewLine}")
        |> Array.skip 1
        |> Array.map (fun substr ->
            let lines = substr.Split(Environment.NewLine)
            let src,dst = 
                lines.[0].Split(' ').[0]
                |> _.Split("-to-") 
                |> fun arr -> arr.[0], arr.[1]
            
            {
                From = src
                To = dst
                Entries = 
                    lines 
                    |> Array.skip 1
                    |> Array.map (fun line -> 
                        let nums = line.Split(' ')
                        let dstStart = int64 nums.[0]
                        let srcStart = int64 nums.[1]
                        let length = int64 nums.[2]
                        {
                            Dst = { Start = dstStart; Stop = dstStart + length }
                            Src = { Start = srcStart; Stop = srcStart + length }
                            Length = int64 nums[2]
                        })
            })

    seedRanges, maps

let seedToLocationFactory (maps : MapIndex[]) =
    let mapSrcIndex = 
        maps |> Array.map (fun m-> m.From, m ) |> readOnlyDict

    let rec nextMap current (map: MapIndex) = 
        let target = 
            map.Entries 
            |> Array.tryFind (fun e -> e.Src.Contains(current))
            |> Option.map (fun e -> e.Dst.Start + current - e.Src.Start  )
            |> function
            | Some x -> x
            | None -> current

        if map.To = "location" then target
        else nextMap target mapSrcIndex.[map.To]

    fun seed -> nextMap seed mapSrcIndex.["seed"]


let minLocation path = 
    let seeds, maps = parse path
    let seedToLocation = seedToLocationFactory maps
    seeds
    |> Array.Parallel.map (fun seed -> 
        let sw = Stopwatch()
        sw.Start()
        let result = seed.Seeds |> Seq.map seedToLocation |> Seq.min
        printfn "%A finished at %A" seed sw.Elapsed
        result )
    |> Array.min
        
"./puzzle.example"
|> minLocation 
|> Global.shouldBe 46L

"./puzzle.input"
|> minLocation 
|> printfn "The lowest location number that corresponds to any of the initial seed numbers is %i"
