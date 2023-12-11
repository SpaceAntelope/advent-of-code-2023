. $PSScriptRoot\races-common.ps1

$data = Get-Content $PSScriptRoot/puzzle.input

$races = parse $data

$races | ForEach-Object { findTimeBF $_ } | ForEach-Object { $product = 1 } { $product = $product * $_ } { $product }
