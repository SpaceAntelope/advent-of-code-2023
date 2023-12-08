
function parse ([string]$data) {
    $seeds = [regex]::match($data, "(?<=seeds: )(\d+\s?)+").Value.trim() -split "\s+" | ForEach-Object { [long]$_ }
    $index = @{ seeds = $seeds }

    switch ($data -split "`n" 
        | Select-Object -Skip 1
        | Where-Object { -not [string]::IsNullOrWhiteSpace($_) } 
        | ForEach-Object trim) {
        { $_ -match "map:" } {            
            $currentMap = $_ -split "\s+" | Select-Object -First 1
            $index[$currentMap] = @()
            continue;
        }
        default {
            $dst, $src, $range = $_ -split "\s+" | ForEach-Object { [long]$_ }
            $index[$currentMap] += [PSCustomObject]@{
                SourceStart = $src
                TargetStart = $dst
                Range       = $range
            }
        }
    }
    
    $index
}

function getTarget($source, [array]$mapEntries) {
    $map = $mapEntries 
    | Where-Object { $source -ge $_.SourceStart -and $source -lt ($_.SourceStart + $_.Range) } 
    | Select-Object -f 1

    if ($map) {
        $map.TargetStart + ($source - $map.SourceStart)
    }
    else {
        $source
    }
}

$supplyChain = "seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water", "water-to-light", "light-to-temperature", "temperature-to-humidity", "humidity-to-location"

function fromSeedToLocation($seed, $index) {
    
    $supplyChain 
    | ForEach-Object `
        { $source = $seed } `
        { $source = getTarget $source $index[$_] } `
        { $source }
}

function parseSeedRanges($seeds) {
    $seeds 
    | ForEach-Object `
        -begin { $chunk = @() } `
        -process { if ($chunk.length -eq 2) { , $chunk; $chunk = @($_) } else { $chunk += $_ } } `
        -end { , $chunk } 
}