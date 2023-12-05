$testData = @(
    [pscustomobject]@{ Line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"; Possible = $true }
    [pscustomobject]@{ Line = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"; Possible = $true }
    [pscustomobject]@{ Line = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"; Possible = $false }
    [pscustomobject]@{ Line = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"; Possible = $false }
    [pscustomobject]@{ Line = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"; Possible = $true }
)

$availableCubes = @{
    red   = 12
    green = 13
    blue  = 14
}

function parse($str) {
    [regex]::Matches($str, "(?<num>\d+) (?<word>\w+)|(?<word>\w+) (?<num>\d+)")
    | Select-Object @{l = 'Color'; e = { $_.Groups["word"].Value } }, @{l = 'Count'; e = { [int]$_.Groups['num'].Value } }
}

function isGamePossible ($source) {
    $game = parse $source
    $notPossibleSets = $game
    | Where-Object Color -ne "Game" 
    | Where-Object { $availableCubes[$_.Color] -lt $_.Count }

    if ($notPossibleSets) {
        0
    }
    else {
        $game | Where-Object Color -eq 'Game' | ForEach-Object Count
    }    
}

#$testData | ? { $_.Possible -eq (isGamePossible $_.Line) }

$testResult = $testData |  Measure-Object { isGamePossible $_.Line } -sum

if ($testResult.sum -ne 8) {
    throw "test failed"
}

gc .\puzzle.input | Measure-Object { isGamePossible $_ } -sum
#parse $testData[0].Line | Select-Object @{l = 'Color'; e = { $_.Groups["word"].Value } }, @{l = 'Count'; e = { [int]$_.Groups['num'].Value } }