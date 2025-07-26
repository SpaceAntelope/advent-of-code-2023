namespace Day20.Tests

module Common = 
    open Day20.Model

    let pulseParse str = match str with "High" -> High | "Low" -> Low | x -> failwith $"{x} is not a pulse"
    let stateParse str = match str with "Off" -> Off | "On" -> On | x -> failwith $"{x} is not a flip state"
