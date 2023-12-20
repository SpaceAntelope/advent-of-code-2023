open System
open System.Text.RegularExpressions

type Condition = 
    | Operational
    | Damaged
    | Unknown


let parse (data:string) =
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map (fun line -> 
        let parts = line.Split(' ')
        {| Springs = parts.[0].ToCharArray() |> Array.map (function '.' -> Operational | '#' -> Damaged | _ -> Unknown)
           Source = parts.[0].ToCharArray()
           CRC = parts.[1].Split(',') |> Array.map int |}
        )

let validatebyCRC (crc: int[]) (springs: string) = 
    Regex.Matches(springs, "#+")
    |> Seq.cast<Match> 
    |> Seq.map _.Length
    |> Array.ofSeq
    |> fun x -> x = crc

let validateByMask (mask: string) (springs: string) = 
    mask.Substring(0,springs.Length).ToCharArray() 
    |> Array.zip (springs.ToCharArray())
    |> Array.forall (
        function 
        | '.','.' 
        | '#','#'
        | _, '?' -> true
        | _ -> false)

let generateArrangementFromIslandIndices (indices: int seq) (crc :int[]) (maskLength: int) =
    let result = Array.create maskLength '.' 
    let indexCount = indices |> Seq.length
    let partialCRC = crc |> Seq.take indexCount
    indices 
    |> Seq.zip partialCRC
    |> Seq.iter (fun (length, index) -> 
        [index..(index+length-1)] 
        |> Seq.iter (fun i -> result.[i] <- '#'))
    
    String(result)

(* Generate springs by moving around damaged islands, every recursion moving to the next island, 
 * possible starting index for consequent islands recalculated on every step
 *)
let permutations(mask: string) (crc: int[]) =
    let totalIslandCount = crc.Length
    let validateByMask start length = 
        printfn "total %d start %d length %d" mask.Length start length
        mask.Substring(start, length).ToCharArray() |> Array.forall (fun c -> c <> '.')
        && (start = 0 || mask.Chars(start-1) <> '#')
        && (start + length = mask.Length || mask.Chars(start+length) <> '#' )

    let rec search (minIndex: int) (islandIndices: int list) =  
        seq {      
            let placedIslandsCount = islandIndices.Length

            if placedIslandsCount = totalIslandCount 
            then yield generateArrangementFromIslandIndices islandIndices crc mask.Length
            else
                let currentIslandLength = crc[placedIslandsCount]
                let minDotsNeededToSeparateRemainingIslandsFromEachOther = totalIslandCount - placedIslandsCount - 1 // including current island
                let remainingIslandsLength = crc |> Array.skip (placedIslandsCount+1) |> Array.sum // not including current island
                let maxIndexForCurrentIsland = mask.Length - minDotsNeededToSeparateRemainingIslandsFromEachOther - remainingIslandsLength - 1
                for currentIslandIndex in minIndex..maxIndexForCurrentIsland do
                    if validateByMask currentIslandIndex currentIslandLength
                    then
                        let minIndexForNextIsland = currentIslandIndex + currentIslandLength + 1                            
                        yield! search minIndexForNextIsland (islandIndices@[currentIslandIndex])
        }

    search 0 []