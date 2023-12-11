. $PSScriptRoot\races-common.ps1

$data = Get-Content $PSScriptRoot/puzzle.input -raw

$race = parse2 $data

quadraticSolve $race
