. .\scratchcards-common.ps1

calculate (Get-Content .\puzzle.input -raw) | Measure-Object Points -sum