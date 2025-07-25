namespace Day20

module Model =

    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open System.Text

    type Pulse = High | Low
    type FFState = On | Off 
        with static member Invert (state: FFState) = 
                match state with On -> Off | Off -> On

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

        member x.UpdateDestinations(network : IReadOnlyDictionary<string, ModuleBase>) =
            let dst = x.DestinationModules |> Array.map (fun x-> network.[x])
            let output = x.Output()
            match output with 
            | High -> dst |> Array.iter _.SendHighPulseFrom(x.Name)
            | Low -> dst |> Array.iter _.SendLowPulseFrom(x.Name)

            // dst |> Array.map(fun node -> sprintf "%s --%A-> %s hi: %d lo: %d" x.Name output node.Name node.HighPulseCount node.LowPulseCount)
            dst |> Array.map(fun node -> sprintf "%s --%A-> %s" x.Name output node.Name)


    type FlipFlop(name: string, destinationModules: string[]) = 
        inherit ModuleBase(name, destinationModules)

        member val State = Off with get, set

        override x.SendHighPulseFrom(name:string) =
            x.IncreaseHighPulseCount()

        override x.SendLowPulseFrom(name:string) = 
            x.IncreaseLowPulseCount()
            x.State <- FFState.Invert x.State
            x.PulsePending <- true
        
        override x.Output() =
            x.PulsePending <- false
            match x.State with
                | On -> High
                | Off -> Low

    type Conjunction(name: string, destinationModules: string[]) = 
        inherit ModuleBase(name, destinationModules)

        let state = Dictionary<string, Pulse>()
        member x.State with get() = state

        member x.RegisterInput(name:string) = state.Add(name, Low)
        
        override x.SendHighPulseFrom(name: string) =
            x.PulsePending <- true
            x.IncreaseHighPulseCount()
            state.[name] <- High
            
        override x.SendLowPulseFrom(name: string) =
            x.PulsePending <- true
            x.IncreaseLowPulseCount()
            state.[name] <- Low

        override x.Output() =
            x.PulsePending <- false
            state.Values 
            |> Seq.forall (fun x -> x = High)
            |> function
            | true -> Low
            | _ -> High

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
        override x.SendHighPulseFrom(name: string) = ()
        override x.SendLowPulseFrom(name: string) = ()
        override x.Output() = 
            x.PulsePending <- false
            Low
            