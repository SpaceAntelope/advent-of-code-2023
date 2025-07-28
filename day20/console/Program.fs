namespace Day20

open Model

module Program =
    open System.Reflection
    open System.IO
    open System.Text

    let part1 () =
        let inputPath =
            Path.Join(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "./input/puzzle.input")

        let network, index = Day20.Part1.parse inputPath
        let printNetowrkState() = network |> Array.iter (printfn "%O")
        
        let iterations = 1000
        let sb = StringBuilder()
        let log : string -> unit = 
            if iterations < 15 then
                fun msg -> sb.AppendLine(msg) |> ignore
            else
                fun msg -> ()

        let edges = ResizeArray<PulsingEdge[]>()

        for i in 1 .. iterations do
            let result = Day20.Part1.processPulses' network
            edges.AddRange(result)

            if iterations < 10
            then
                printfn $"\n-- Button press #{i} --"
                result 
                |> Seq.iteri (fun step edges -> 
                    printfn "\nStep: %d.%d" i (step+1)
                    edges |> Array.iter (fun e -> printfn "%O :: %O" e index.[e.Target]))


        let evaluation = Day20.Part1.evaluate network

        printfn
            "If you multiply the total number of low pulses sent by the total number of high pulses sent you get %A"
            evaluation

        // let counts = edges |> Seq.countBy _.Pulse |> Array.ofSeq
        // printfn "%A %d" counts (snd counts.[0]* snd counts.[1])

    [<EntryPoint>]
    let main argv =


        part1 ()

        1
