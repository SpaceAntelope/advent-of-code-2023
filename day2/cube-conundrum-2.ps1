$testData = @(
    [pscustomobject]@{ Line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"; MinimumInventory = "4 red, 2 green, 6 blue" }
    [pscustomobject]@{ Line = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"; MinimumInventory = "1 red, 3 green, 4 blue" }
    [pscustomobject]@{ Line = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"; MinimumInventory = "20 red, 13 green, 6 blue" }
    [pscustomobject]@{ Line = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"; MinimumInventory = "14 red, 3 green, 15 blue" }
    [pscustomobject]@{ Line = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"; MinimumInventory = "6 red, 3 green, 2 blue" }
)

function parse($str) {
    [regex]::Matches($str, "(?<num>\d+) (?<word>\w+)|(?<word>\w+) (?<num>\d+)")
    | Select-Object @{l = 'Color'; e = { $_.Groups["word"].Value } }, @{l = 'Count'; e = { [int]$_.Groups['num'].Value } }
}

function minimumViableInventory([string]$source) {
    $game = parse $source

    $game
    | Where-Object Color -ne "Game" 
    | Group-Object Color
    | ForEach-Object { $_.Group | Sort-Object Count | Select-Object -last 1 }
}

function power ($cubes) {
    $cubes | ForEach-Object { $product = 1 } { $product = $product * $_.Count } { $product }
}

$testPower = $testData | ForEach-Object { power (minimumViableInventory $_.Line) } | Measure-Object -sum

if ($testPower -ne 2286) { throw 'test failed' }

Get-Content .\puzzle.input.txt | Measure-Object { power (minimumViableInventory $_ ) } -sum
