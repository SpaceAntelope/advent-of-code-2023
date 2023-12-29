open System
open System.Collections.Generic

let hash =
    let cache = Dictionary<string, int>()

    fun (source: string) ->
        if cache.ContainsKey source then
            cache.[source]
        else
            source.ToCharArray()
            |> Array.map int
            |> Array.fold (fun state current -> ((state + current) * 17) % 256) 0
            |> fun h ->
                    cache.Add(source, h)
                    h

type Lens = { Label: string; Focal: int }
type Boxes = ResizeArray<Lens> array
let lens label focal = {Label = label; Focal = focal }
let initBoxes() : Boxes = Array.init 256 (fun _ -> ResizeArray<Lens>())

let updateBoxes (boxes: Boxes) (source:string) =

    let parts = source.Split([|'=';'-'|], StringSplitOptions.RemoveEmptyEntries)
    let label = parts[0]
    let box = boxes[hash label]

    if parts.Length = 1 
    then 
        box 
        |> Seq.tryFindIndex (fun lens -> lens.Label = label)
        |> Option.iter box.RemoveAt
    else
        box 
        |> Seq.tryFindIndex (fun lens -> lens.Label = label)
        |> function
        | Some index -> 
            box.RemoveAt index
            box.Insert(index, lens label (int parts[1]))
        | None  -> box.Add(lens label (int parts[1]))

let boxes2str (boxes: ResizeArray<Lens>[]) =
    let lens2str (lens:Lens) = $"{lens.Label} {lens.Focal}"
    let box2str (box: ResizeArray<Lens>) = 
        box 
        |> Seq.map lens2str 
        |> Seq.fold (sprintf "%s [%s]") String.Empty
    boxes 
    |> Array.mapi (fun i b -> i,b)
    |> Array.filter (fun (index,box) -> box.Count > 0)
    |> Array.map (fun (index,box) -> $"{index}: {box2str box}")

let focusingPower (boxes: Boxes) = 

    let fp (box : ResizeArray<Lens>) =
        box 
        |> Seq.mapi (fun i l -> i+1,l)
        |> Seq.fold (fun state (slot,lens) -> state + slot * lens.Focal ) 0

    boxes
    |> Array.mapi (fun i box -> i,box)
    |> Array.fold (fun state (index,contents) -> state + (index + 1) * fp contents) 0
