. .\seed-common.ps1

$index = parse (Get-Content .\puzzle.input -raw)
$index["seeds"] 
| Measure-Object { fromSeedToLocation $_ $index } -min 
| ForEach-Object Minimum