function calculate($str) {
    $str -split "`n" | ForEach-Object {
        $card, $winningNumbers, $playingNumbers = $_ -split "[:|]" | ForEach-Object trim
        $winningNumbers = $winningNumbers -split "\s+"
        $playingNumbers = $playingNumbers -split "\s+"
        $matchCount = Compare-Object $playingNumbers $winningNumbers -IncludeEqual -ExcludeDifferent | Measure-Object | ForEach-Object count
        [pscustomobject]@{
            Card       = $card 
            Playing    = $playingNumbers
            Winning    = $winningNumbers
            Points     = [int][math]::pow(2, $matchCount - 1) 
            MatchCount = $matchCount
        }
    }
}
