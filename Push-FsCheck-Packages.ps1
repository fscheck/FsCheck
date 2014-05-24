Nuget update -self
echo "reminder - have you done:"
echo "NuGet SetApiKey Your-API-Key (to make sure you can push)"
echo "This script will push FsCheck and FsCheck.Xunit to the public NuGet repo."
$choice = ""
while ($choice -notmatch "[y|n]"){
    $choice = read-host "Sure? (Y/N)"
    }
if ($choice -eq "y"){
    NuGet Push FsCheck.0.9.4.0.nupkg
    NuGet Push FsCheck.Xunit.0.4.1.0.nupkg
    }
    
else {write-host "Aborted."}