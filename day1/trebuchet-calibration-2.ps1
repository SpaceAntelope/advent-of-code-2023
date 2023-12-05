# $ErrorActionPreference = 'stop'

$idx = @{
    one   = "1"
    two   = "2"
    three = "3"
    four  = "4"
    five  = "5"
    six   = "6"
    seven = "7"
    eight = "8"
    nine  = "9"    
}

function allIndexes($str, $substr) {    
    $lastIndex = -1
    do {
        $index = $str.indexof($substr, $lastIndex + 1)
        $index
        $lastIndex = $index
    } while ($index -ge 0)
}

function firstAndLast($source) {
    $WrittenInFull = $idx.keys 
    | ForEach-Object { 
        $key = $_
        allIndexes $source $_ 
        | ForEach-Object { [pscustomobject]@{ 
                Value = $idx[$key]; 
                Index = $_
            } 
        } 
    }
    | Where-Object index -ge 0
    
    $Numerals = [regex]::matches($source, "\d") | Select-Object Value, Index

    $indices = ($WrittenInFull, $Numerals) | ForEach-Object { $_ } | Sort-Object index

    $left = $indices | Select-Object -f 1 | ForEach-Object value 
    $right = $indices | Select-Object -l 1 | ForEach-Object value

    $left, $right
}

filter extract { 
    $left, $right = firstAndLast $_.source
    $result = $left + $right

    if ($_.result -and $result -ne $_.result) {
        throw "Expected $($_.result) but found $result in $($_.Source)"
    }

    [pscustomobject]@{
        source = $_.source
        left   = $left 
        right  = $right
        sum    = [convert]::ToInt32($result)
    }
}

@(
    @{ source = 'two1nine'; result = '29' }
    @{ source = 'eightwothree'; result = '83' }
    @{ source = 'abcone2threexyz'; result = '13' }
    @{ source = 'xtwone3four'; result = '24' }
    @{ source = '4nineeightseven2'; result = '42' }
    @{ source = 'zoneight234'; result = '14' }
    @{ source = '7pqrstsixteen'; result = '76' }
    @{ source = 'eighthree'; result = '83' }
    @{ source = 'seven'; result = '77' }
    @{ source = 'twone'; result = '21' }
    @{ source = 'sevenineight'; result = '78' }
    @{ source = 'ftwonevbhlhlqhsix5dzfqgsone'; result = '21' }
) | extract | out-null

get-content .\puzzle.input.txt | Select-Object @{l = "source"; e = { $_ } } | extract

