function calculate([string]$data) {
    $data -split "`n" | ForEach-Object {
        $card, $winningNumbers, $playingNumbers = $_ -split "[:|]" | ForEach-Object trim
        $winningNumbers = $winningNumbers -split "\s+"
        $playingNumbers = $playingNumbers -split "\s+"
        $matchCount = Compare-Object $playingNumbers $winningNumbers -IncludeEqual -ExcludeDifferent | Measure-Object | ForEach-Object count
        
        [pscustomobject]@{
            Index      = [int]($card -split "\s+" | Select-Object -last 1)
            Points     = [int][math]::pow(2, $matchCount - 1) 
            MatchCount = $matchCount
        }
    }
}

function calculateAccumulatedCards($data) {
    $cards = calculate $data # do pt1 calculations

    $cards 
    | Where-Object MatchCount -gt 0 # otherwise the losing card becomes its own child and the search recycles to overflow
    | ForEach-Object { 
        $children = ($_.Index + 1) .. ($_.Index + $_.MatchCount)  # range of numbers corresponding to indices of cards won
        | ForEach-Object { $cards[$_ - 1] } # map to the actual cards
        | Where-Object { $null -ne $_ }  # filter out overflow when index exceeds input length

        $_ | Add-Member -NotePropertyName Children -NotePropertyValue $children # add cards gained as children property
    }

    # do depth first search on every card and its branching children while counting every node
    # the recursive function is inlined in the foreach block because it's simpler than referencing it 
    # from outside the parallel scope
    $cards | ForEach-Object -Parallel {
        function traverse($card) {
            $script:count++        
            foreach ($c in $card.Children) {
                traverse($c)
            }
        }
        
        $script:count = 0
        traverse $_ 
        $script:count # pass node count to     
    } 
    | Measure-Object -sum    
    | ForEach-Object sum
}