. .\scratchcards-common.ps1

$cards = calculate (Get-Content .\puzzle.input -raw) 

# $cardCount = 0
# $cardStack = @()

# for ($i = 0; $i -lt $cards.Count; $i++) {
#     $card = $cards[$i]
#     $cardStack.Add($card)

#     $nextCards = $cards[($i + 1)..($i + $card.MatchCount)]
# }

# $cards | % { $cardCount = 0; $index = } { 
#     $cardCount++

# }