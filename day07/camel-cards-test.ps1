$testdata = @"
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"@

$suite = "AKQJT98765432".ToCharArray() | ForEach-Object ToString



$rankedHandIndex = @(
    "Highest",
    "TwoOfAKind",
    "TwoPairs",
    "ThreeOfAKind",
    "FullHouse",
    "FourOfAKind",
    "FiveOfAKind"
)

function parse([string]$data) {
    $data -split "`n" | ForEach-Object { $hand, $bet = $_.trim() -split "\s+"; [pscustomobject]@{ Hand = $hand; Bet = [double]$bet } }
}

function rankHandByType([string]$hand) {
    $cardCounts = $hand.ToCharArray() | Group-Object | ForEach-Object count | Sort-Object -Descending
    switch (,$cardCounts) {
        { $_[0] -eq 5 } { 7;  continue }
        { $_[0] -eq 4 } { 6; continue }
        { $_[0] -eq 3 -and $_[1] -eq 2 } { 5; continue }
        { $_[0] -eq 3 } { 4; continue }
        { $_[0] -eq 2 -and $_[1] -eq 2 } { 3; continue }
        { $_[0] -eq 2 } { 2; continue }
        default { 1 }
    }
}

function rankCards($card) {
    $suite.Length - $suite.indexOf($card)
}

parse $testdata | Tee-Object -var k | ForEach-Object Hand | ForEach-Object { rankHandByType $_ } | ForEach-Object { $rankedHandIndex[$_-1] }

$suite | ForEach-Object{ rankCards $_}

function compareHands($hand1, $hand2) {
    $type1 = rankHandByType $hand1
    $type2 = rankHandByType $hand2

    if ($type1 -gt $type2) {
        1
    } elseif ($type1 -lt $type2) {
        -1
    } else {
        $firstDifferentlyRankedCards = 0..($hand1.length-1) 
        | ForEach-Object {            ,$hand1[$_],$hand2[$_]        } 
        | Where-Object { (rankCards $_[0]) -ne (rankCards $_[1]) } 
        | Select-Object -first 1

        if (-not $firstDifferentlyRankedCards) {
            0
        } elseif ($firstDifferentlyRankedCards[0] -gt $firstDifferentlyRankedCards[1]) {
            1
        } else {
            -1
        }
    }
}

function calculateTotalWinninds([array]$hands) {

}