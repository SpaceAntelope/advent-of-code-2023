[CmdletBinding()]
param (
    [Parameter(Mandatory)]
    [string]
    $InputPath
)

$lines = Get-Content $InputPath

$shapes = $lines 
| Select-String "^[^\s]+" 
| ForEach-Object { $_.Matches.value } 
| Sort-Object -Unique 
| ForEach-Object {
    $name = $_.trim("&%")
    switch ($_.TocharArray()[0]) {
        "%" { "$name [shape=circle]`n" }
        "&" { "$name [shape=box]`n" }
        default { "$name [shape=doublecircle]`n" }
    }
}

$edges = $lines 
| ForEach-Object Trim("&%") 
| ForEach-Object { $_ -replace "(?<=->\s*)(.+`$)", " { `$1 }`n" }

@"
digraph { 
    $shapes
    
    $edges
}
"@

# .\input2dot.ps1 .\console\input\puzzle.input > puzzle.input.dot 