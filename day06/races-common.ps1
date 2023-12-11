function parse($data) {
    $time, $distance = $data -split "\n" | ForEach-Object { , ($_ -replace "\w+:\s*|\s`$" -split "\s+" | ForEach-Object { [int]$_.trim() }) } 

    0..($time.length - 1) | ForEach-Object {

        [PSCustomObject]@{
            Time     = [convert]::ToDouble($time[$_])
            Distance = [convert]::ToDouble($distance[$_])
        }
    }
}


function parse2($data) {
    $time, $distance = $data -split "`n" | ForEach-Object { $_ -replace "\D" } | ForEach-Object { [convert]::ToDouble($_) }
    [pscustomobject]@{
        Time = $time
        Distance = $distance
    }
}

# brute force a solution
function findTimeBF($race) {    
    $minSpeed = [math]::Ceiling($race.Distance / $race.Time)

    $minSpeed..($race.Time - 1) | ForEach-Object { $_ * ($race.Time - $_); } | Where-Object { $_ -gt $race.Distance } | Measure-Object | ForEach-Object count
}

function isInt($num) { $num -eq [long]$num }

# solve ( t - v ) * v = s for speed (aka charging seconds) at minimum distance using the quadratic formula
function qf($time, $distance, [double]$plusOrMinus = 1.0) {
    ((-$time + $plusOrMinus * [math]::sqrt([math]::pow($time, 2.0) - 4.0 * $distance)) / -2.0)
}
function quadraticSolve($race) {
    $bounds = (qf $race.time $race.distance 1.0), (qf $race.time $race.distance -1.0) | Sort-Object

    $lowerBound = (isInt $bounds[0]) ? $bounds[0] + 1 : [math]::Ceiling($bounds[0])
    $higherBound = (isInt $bounds[1]) ? $bounds[1] - 1 : [math]::Floor($bounds[1])

    [math]::Abs($higherBound - $lowerBound ) + 1 # plus one because solution should be left side inclusive
}    