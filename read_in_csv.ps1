param(
[string]$file,
[string]$type = $null
)

# Write-Host $file
if(!$type){
    Import-Csv $file |Format-Table -AutoSize
}
if($type -eq 1){
    Import-Csv $file |Out-GridView
}
if($type -eq 2){
    Import-Csv $file |Format-List
}

