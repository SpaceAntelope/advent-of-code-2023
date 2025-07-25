namespace Day20

module Part1 = 
    open System.Text.RegularExpressions
    open System.IO
    open Model
    open System.Text

    let parse (path: string) = 
        let parseLine (line:string) : ModuleBase =
            let parts = Regex.Split(line, " -> ")
            let src,dst = parts.[0],parts.[1]
            
            match src, src.Trim([|'%';'&'|]), Regex.Split(dst, ",\\s*") with
            | src, name,  dst when src.StartsWith('%') -> 
                FlipFlop(name, dst) 
            | src, name, dst when src.StartsWith('&') -> 
                Conjunction(name, dst)
            | "broadcaster", _, dst ->
                Broadcaster(dst)
            | x, _, z -> failwithf "Don't know what to do with %s (dst: %A)" x z        

        let activeModules =
            File.ReadAllLines(path)
            |> Array.map parseLine

        let neutralModules = 
            let allSource  = activeModules |> Array.map _.Name
            let allDest = activeModules |> Array.collect _.DestinationModules |> Array.distinct
            allDest
            |> Array.except allSource
            |> Array.map (fun x -> Neutral(x) :> ModuleBase)
        
        let allModules = 
            [|  Button() :> ModuleBase
                yield! activeModules
                yield! neutralModules |]
            
        
        let network =
            allModules
            |> Array.map (fun x -> x.Name, x)
            |> readOnlyDict

        // Register conjunction inputs
        network.Values 
        |> Seq.filter(fun x -> x :? Conjunction)
        |> Seq.cast<Conjunction>
        |> Seq.iter (fun c -> 
                network.Values 
                |> Seq.filter (fun x -> x.DestinationModules |> Array.contains c.Name)
                |> Seq.iter (fun input-> c.RegisterInput(input.Name )))

        // printfn "%s %A" allModules.[0].Name allModules.[0].DestinationModules

        allModules, network
        

    let evaluate (modules: ModuleBase[]) =
        let hiPulseCount = modules |> Array.sumBy _.HighPulseCount
        let loPulseCount = modules |> Array.sumBy _.LowPulseCount

        {| Hi = hiPulseCount; Lo = loPulseCount; Evaluation = hiPulseCount * loPulseCount |}

    let processPulses (log: StringBuilder option) (modules: ModuleBase[])  =
        let network =
            modules
            |> Array.map (fun x -> x.Name, x)
            |> readOnlyDict
    
        let writeLog (msg: string) = 
            match log with
            | Some log -> log.AppendLine(msg) |> ignore
            | None -> ()
            
        let mutable step = 0

        let rec proc (changedModules: ModuleBase[] ) =
            if changedModules.Length > 0 
            then 
                step <- step + 1
                writeLog $"\r\nStep: {step}"
                changedModules
                |> Array.collect _.UpdateDestinations(network)
                |> Array.iter writeLog

                modules 
                |> Array.filter _.PulsePending
                |> proc
                        
        modules
        |> Array.filter _.PulsePending
        |> proc 

        network.["button"].PulsePending <- true