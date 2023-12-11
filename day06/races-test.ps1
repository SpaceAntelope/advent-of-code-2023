$testData = @"
Time:      7  15   30
Distance:  9  40  200
"@

. $PSScriptRoot\races-common.ps1

$races = parse $testdata

function findTime($race) {
    
    $minSpeed = [math]::Ceiling($race.Distance/$race.Time)
    $maxSpeed = $race.Time - 1
    $remainingDistanceAtMaxSpeed = $race.Distance - $maxSpeed

    $minSpeed, $maxSpeed, $remainingDistanceAtMaxSpeed
}


$expectedWaysToWin = 4,8,9
$actualWaysToWin = $races | ForEach-Object { findTimeBF $_ }

if ("$expectedWaysToWin" -ne "$actualWaysToWin") {
    throw "Expected $expectedWaysToWin but got $actualWaysToWin"
} else {
    write-host -f green "Expected ways to win test passed."
}

$actualWaysToWin = $races | ForEach-Object { quadraticSolve $_ }

if ("$expectedWaysToWin" -ne "$actualWaysToWin") {
    throw "Expected $expectedWaysToWin but got $actualWaysToWin"
} else {
    write-host -f green "Expected ways by quadratic equation passed."
}