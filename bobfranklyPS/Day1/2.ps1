function DetermineFuel{
    param(
    [int64]$mass
    )
       ([math]::floor($mass / 3)) -2
}
# Pasted puzzle data into a string array
$array = "
#numbers here
"
$masses = $array.split("`r")

$megaMasses = foreach ($mass in $masses){
    [int64]$ticker = $mass
    [string]$fuelStacks = ""
    
    while ($ticker -gt 0){
        $thisFuel = DetermineFuel -mass $ticker
        if ($thisFuel -gt 0){ $fuelStacks += "$thisFuel," }
        $ticker = $thisFuel
        Write-Verbose $ticker -Verbose
    }
    ($fuelStacks.split(",") | Measure-Object -Sum).sum
}

($megaMasses | Measure-Object -Sum).sum
