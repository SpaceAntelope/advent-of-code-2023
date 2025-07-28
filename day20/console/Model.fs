namespace Day20

module Model =

    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open System.Text

    type Pulse = High | Low
    type FlipState = On | Off 
        with static member Invert (state: FlipState) = 
                match state with On -> Off | Off -> On

    type PulsingEdge = {
        Source: string
        Pulse: Pulse
        Target: string
    } with 
        override x.ToString() = sprintf "%s --%A-> %s" x.Source x.Pulse x.Target
        static member Create source pulse target = { Source = source; Pulse = pulse; Target = target }
        static member Parse(line:string) =
            let parts = line.Trim().Split(' ')
            {   Source = parts.[0]
                Target = parts.[2]
                Pulse = 
                    match parts.[1] with
                    | "-low->" -> Low
                    | "-high->" -> High
                    | x -> failwithf "No idea how to make %s into a pulse" x }
            



    [<AbstractClass>]
    type ModuleBase(name: string, destinationModules: string[]) = 
        member val HighPulseCount = 0 with get, set
        member val LowPulseCount = 0 with get, set
        member val PulsePending = false with get, set

        member x.Name = name
        member x.DestinationModules = destinationModules
        member x.IncreaseHighPulseCount() = x.HighPulseCount <- x.HighPulseCount + 1
        member x.IncreaseLowPulseCount() =  x.LowPulseCount <- x.LowPulseCount + 1

        abstract member Output : unit -> Pulse
        abstract member SendHighPulseFrom : string -> unit
        abstract member SendLowPulseFrom : string -> unit

        member x.UpdateDestinations(network : IReadOnlyDictionary<string, ModuleBase>) : PulsingEdge[] =
            match x.PulsePending with
            | true ->
                let dst = x.DestinationModules |> Array.map (fun x-> network.[x])
                let output = x.Output()
                match output with 
                | High -> dst |> Array.iter _.SendHighPulseFrom(x.Name)
                | Low -> dst |> Array.iter _.SendLowPulseFrom(x.Name)

                let drillDown= 
                    dst 
                    |> Array.collect (fun modBase -> modBase.UpdateDestinations(network))
                // dst |> Array.map(fun node -> sprintf "%s --%A-> %s hi: %d lo: %d" x.Name output node.Name node.HighPulseCount node.LowPulseCount)
                // dst |> Array.map(fun node -> sprintf "%s --%A-> %s" x.Name output node.Name)
                let current = 
                    dst 
                    |> Array.map(fun node -> { Source = x.Name; Pulse = output; Target = node.Name})
                
                //|> fun x -> x |> Array.iter (printfn "%O"); x

                drillDown |> Array.append current
            | _ -> [||]

        override x.ToString (): string = 
            sprintf $"{x.GetType().Name,-11} {x.Name} Hi: {x.HighPulseCount,4} Lo: {x.LowPulseCount,4} Pending: {x.PulsePending} -> %A{x.DestinationModules}"

    type FlipFlop(name: string, destinationModules: string[]) = 
        inherit ModuleBase(name, destinationModules)

        member val State = Off with get, set

        override x.SendHighPulseFrom(name:string) =
            x.IncreaseHighPulseCount()

        override x.SendLowPulseFrom(name:string) = 
            x.IncreaseLowPulseCount()
            x.State <- FlipState.Invert x.State
            x.PulsePending <- true
        
        override x.Output() =
            x.PulsePending <- false
            match x.State with
                | On -> High
                | Off -> Low            

    type Conjunction (name: string, destinationModules: string[], state: Dictionary<string,Pulse>) = 
        inherit ModuleBase(name, destinationModules)
        
        new(name:string, destinationModules: string[]) = 
            Conjunction(
                name, 
                destinationModules,
                Dictionary<string, Pulse>())
            
        member val State = state with get, set

        member x.RegisterInput(name:string) = state.Add(name, Low)
        
        override x.SendHighPulseFrom(name: string) =
            x.IncreaseHighPulseCount()
            x.PulsePending <- true
            state.[name] <- High
            
        override x.SendLowPulseFrom(name: string) =
            x.IncreaseLowPulseCount()
            x.PulsePending <- true
            state.[name] <- Low

        override x.Output() =
            x.PulsePending <- false
            state.Values 
            |> Seq.forall (fun x -> x = High)
            |> function
            | true -> Low
            | _ -> High

        override x.ToString (): string = 
            sprintf $"{base.ToString()} %A{state.Keys}"

    type Broadcaster(destinationModules: string[]) = 
        inherit ModuleBase("broadcaster", destinationModules)

        member val State = Low with get, set

        override x.Output() =
            x.PulsePending <- false
            x.State

        override x.SendHighPulseFrom(name: string) =
            x.IncreaseHighPulseCount()
            x.PulsePending <- true
            x.State <- High

        override x.SendLowPulseFrom(name: string) =
            x.IncreaseLowPulseCount()
            x.PulsePending <- true
            x.State <- Low

    type Neutral(name:string) = 
        inherit ModuleBase(name, [||])
        member val State : Pulse option = None with get, set
        override x.SendHighPulseFrom(name: string) =
            x.IncreaseHighPulseCount()
            // x.PulsePending <- true
            x.State <- Some High

        override x.SendLowPulseFrom(name: string) =
            x.IncreaseLowPulseCount()
            // x.PulsePending <- true
            x.State <- Some Low

        override x.Output() = 
            // x.PulsePending <- false
            match x.State with
            | Some pulse -> pulse
            | None -> failwith "Output of unset neutral module requested"

    type Button() = 
        inherit ModuleBase("button", [|"broadcaster"|])
        do base.PulsePending <- true
        override x.SendHighPulseFrom(name: string) = failwith $"You can't high pulse a button, {name}."
        override x.SendLowPulseFrom(name: string) = failwith $"You can't low pulse a button, {name}."
        override x.Output() = 
            x.PulsePending <- false
            Low
            