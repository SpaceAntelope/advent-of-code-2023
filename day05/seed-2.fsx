open System
open System.IO
open System.Collections.Generic
open System.Collections
open System.Text.RegularExpressions

let data = File.ReadAllLines("./puzzle.input")
let testData =  
    File.ReadAllLines("./seed-test.ps1") 
    |> Array.skip 1 
    |> Array.take 32 
    |> Array.map _.Trim()    
    |> Array.filter (not<<String.IsNullOrWhiteSpace)

type Range = { 
    Map: string
    First: int64
    Last: int64
}

type MapEntry ={ Source: Range; Target: Range }

printfn "%A" testData

let parseRanges (data : string[]) =
    let index = Dictionary<string,ResizeArray<MapEntry>>()
    let line2num (line:string) = Regex.Split(line,"\\s+") |> Array.map int64

    let mapLineToRange (mapName: string) (line:string) = 
        line.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64
        |> fun arr -> {
            Source = {
                Map = mapName
                First = arr.[1]
                Last = arr.[1] + arr.[2] - 1L }
            Target = {
                Map = mapName
                First = arr.[0]
                Last = arr.[0] + arr.[2] - 1L
            }
        }
        

    let seeds = 
        data
        |> Array.head
        |> _.Split(' ')
        |> Array.skip 1
        |> Array.chunkBySize 2
        |> Array.map (fun range -> { Map = "seeds"; First = int64 range.[0]; Last = int64 range.[0] + int64 range.[1] - 1L })
    
       
    data
    |> Array.skip 1    
    |> Array.fold (fun (idx: Dictionary<string,ResizeArray<MapEntry>>,currentMapName) line ->
        match line with
        | line when line.EndsWith("map:") -> 
            let newMapName = line.Replace(" map:", "")
            idx.Add(newMapName, ResizeArray<MapEntry>())
            idx, newMapName
        | line when Regex.IsMatch(line, @"(\d+\s*)+") ->
            idx.[currentMapName].Add(mapLineToRange currentMapName line)
            idx, currentMapName
        | line -> failwith $"Can't handle line '{line}'"
        
        ) (index, String.Empty)
    |> fst


let index = parseRanges testData 
let supplyChain = [|"seed-to-soil"; "soil-to-fertilizer"; "fertilizer-to-water"; "water-to-light" ;"light-to-temperature"; "temperature-to-humidity"; "humidity-to-location"|]

index |> printfn "%A"
index.["seed-to-soil"] |> printfn "%A"

let nextMapIndex = 
    index.Keys 
    |> Seq.fold (fun idx mapName -> 
        let parts = mapName.Split('-')
        index.Keys 
        |> Seq.tryFind (fun x -> x.StartsWith(parts.[2]))
        |> function
        | Some nextMap -> idx |> Map.add mapName nextMap
        | None -> idx
    ) Map.empty

printfn "%A" nextMapIndex

let search (node: MapEntry) = 
    let mapParts = node.Source.Map.Split('-')    
    let nextMap = Map.tryFindKey nextMapIndex.[mapParts[2]] index
    
    let nextRange = nextMap