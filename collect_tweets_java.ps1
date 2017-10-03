# Collect all

$politicians = Import-Csv politicians.csv

# this does the following...
function collect_twitter($user, $since = '2014-09-10', $until = '2017-09-24'){ #, $n = 5
    java -jar got.jar username=$user since=$since until=$until #maxtweets = $n
}

$j = 0
foreach($i in $politicians){
    $j = $j + 1
    write-host 'Run ' $j
    $x = $i.screen_name
    collect_twitter $x
    $df = import-csv output_got.csv -delimiter ';' -encoding UTF7
    $df | ForEach-Object {$_.username = $x; $_} | `
    export-csv data\$x'.csv' -encoding UTF7 -delimiter ';' -NoTypeInformation
    sleep 2
}

$getFirstLine = $true

get-childItem "D:\google drive\projects_personal\GetOldTweets-java\data\*.csv" | foreach {
    $filePath = $_

    $lines =  $lines = Get-Content $filePath  
    $linesToWrite = switch($getFirstLine) {
           $true  {$lines}
           $false {$lines | Select -Skip 1}

    }

    $getFirstLine = $false
    Add-Content "alldata.csv" $linesToWrite
}