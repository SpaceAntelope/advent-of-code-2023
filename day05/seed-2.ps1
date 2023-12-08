. .\seed-common.ps1

$index = parse (Get-Content .\puzzle.input -raw)
parseSeedRanges $index["seeds"] 
| ForEach-Object { 
    $start, $range = $_    
    $stop = $start + $range - 1

    [math]::min((fromSeedToLocation $start $index), (fromSeedToLocation $stop $index))
    # apparently ps range operator (..) only works with int32, go figure
    # for ($i = $start; $i -lt $stop; $i++) { $i }
} #-ThrottleLimit $env:NUMBER_OF_PROCESSORS
| Measure-Object { fromSeedToLocation $_ $index } -min
| ForEach-Object Minimum