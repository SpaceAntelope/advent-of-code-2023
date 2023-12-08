function calculate([string]$data) {
    $data -split "`n" | ForEach-Object {
        $card, $winningNumbers, $playingNumbers = $_ -split "[:|]" | ForEach-Object trim
        $winningNumbers = $winningNumbers -split "\s+"
        $playingNumbers = $playingNumbers -split "\s+"
        $matchCount = Compare-Object $playingNumbers $winningNumbers -IncludeEqual -ExcludeDifferent | Measure-Object | ForEach-Object count
        [pscustomobject]@{
            Card       = $card 
            Index      = [int]($card -split "\s+" | Select-Object -last 1)
            Playing    = $playingNumbers
            Winning    = $winningNumbers
            Points     = [int][math]::pow(2, $matchCount - 1) 
            MatchCount = $matchCount
        }
    }
}

function calculateAccumulatedCards([string]$data) {
    $cards = calculate $data

    $cardIndex = $cards | ForEach-Object { $idx = @{} } { $idx[$_.Index] = $_ } { $idx }

    $cards 
    | Where-Object MatchCount -gt 0
    | ForEach-Object { 
        $children = ($_.Index + 1) .. ($_.Index + $_.MatchCount) | ForEach-Object { $cardIndex[$_] } | Where-Object { $null -ne $_ }
        $_ | Add-Member -NotePropertyName Children -NotePropertyValue $children
    }

    $cards | forEach-Object -para {
        $script:count = 0
        function traverse($card) {
            $script:count++        
            foreach ($c in $card.Children) {
                traverse($c)
            }
        }

        write-host -f blue Starting $_.card with $_.Children.Count children
        traverse $_ 
        $script:count 
        write-host -f green Finished $_.card and got $script:count total
    } -ThrottleLimit 10
    | Measure-Object -sum    
    | ForEach-Object sum
}