. .\scratchcards-common.ps1

$data = Get-Content .\puzzle.input -raw
$count = calculateAccumulatedCards $data

"Accumulated a total of $count cards."