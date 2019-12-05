$array = "
# Pasted Input here
"

function DetermineFuel{
    param(
    [int]$mass
    )
       ([math]::floor($mass / 3)) -2
}


$masses = foreach ($mass in $array.split("`r")){
    DetermineFuel -mass $mass
}
$masses | Measure-Object -Sum | select-object sum
