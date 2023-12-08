$testData = @"
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"@

. .\seed-common.ps1

$index = parse $testData

# test 1
$expectedLocations = 82, 43, 86, 35
$actuallocations = $index["seeds"] | ForEach-Object { fromSeedToLocation $_ $index }

if (Compare-Object $expectedLocations $actuallocations) {
    throw "Expected ($expectedLocations) but got $actuallocations"
}
else { write-host -f green "Location matching passed" }

# test 2
$expectedLowestLocationNumber = 35
$actualLowestLocationNumber = $index["seeds"] | Measure-Object { fromSeedToLocation $_ $index } -min | ForEach-Object Minimum

if ($expectedLowestLocationNumber -ne $actualLowestLocationNumber) {
    throw "Expected [$expectedLowestLocationNumber] but got [$actualLowestLocationNumber]"
}
else { write-host -f green "Lowest location finding passed" }

# test 3
$expectedLowestLocationNumber = 46
$actualLowestLocationNumber = parseSeedRanges $index["seeds"] 
| ForEach-Object { 
    $start, $range = $_
    $start .. ($start + $range - 1) 
} 
| Measure-Object { fromSeedToLocation $_ $index } -min
| ForEach-Object Minimum

if ($expectedLowestLocationNumber -ne $actualLowestLocationNumber) {
    throw "Expected [$expectedLowestLocationNumber] but got [$actualLowestLocationNumber]"
}
else { write-host -f green "Extended seed range lowest location finding passed" }


$expectedLowestLocationNumber = 46
$actualLowestLocationNumber = parseSeedRanges $index["seeds"] 
| ForEach-Object { 
    $start, $range = $_    
    $stop = $start + $range - 1

    [math]::min((fromSeedToLocation $start $index), (fromSeedToLocation $stop $index))
    # apparently ps range operator (..) only works with int32, go figure
    # for ($i = $start; $i -lt $stop; $i++) { $i }
} #-ThrottleLimit $env:NUMBER_OF_PROCESSORS
| Measure-Object -min
| ForEach-Object Minimum

if ($expectedLowestLocationNumber -ne $actualLowestLocationNumber) {
    throw "Expected [$expectedLowestLocationNumber] but got [$actualLowestLocationNumber]"
}
else { write-host -f green "Extended seed range lowest location finding passed" }