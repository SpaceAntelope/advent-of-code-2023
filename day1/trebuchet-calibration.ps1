get-content .\puzzle.input
| ForEach-Object { 
    $x = $_ -replace "\D"

    $left = $x[0]
    $right = $x[-1]

    [pscustomobject]@{
        source = $_ 
        left   = $left 
        right  = $right
        sum    = [int]($left + $right)
    }
} | Measure-Object sum -sum