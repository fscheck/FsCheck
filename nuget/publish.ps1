echo "reminder - have you done:"
echo "..\.nuget\NuGet.exeNuGet SetApiKey Your-API-Key (to make sure you can push)"
echo "This script will push FsCheck and FsCheck.Xunit to the public NuGet repo."
$choice = ""
while ($choice -notmatch "[y|n]"){
    $choice = read-host "Sure? (Y/N)"
    }
if ($choice -eq "y"){
    ..\.nuget\NuGet.exe Push ..\bin\FsCheck.?.?.?.nupkg
    ..\.nuget\NuGet.exe Push ..\bin\FsCheck.Xunit.?.?.?.nupkg
    }
    
else {write-host "Aborted."}