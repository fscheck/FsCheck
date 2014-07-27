param($installPath, $toolsPath, $package, $project)

write-host "Install path" $installPath
$packagesFolder = Split-Path -Path $installPath -Parent
write-host "packages folder" $packagesFolder
write-host $toolsPath
write-host $package
write-host $project
$NunitRunners = join-path -path $packagesFolder -childpath "NUnit.Runners.2.6.3"

$project.Object.References.Add($NunitRunners+"\nunit.core")
$project.Object.References.Add($NunitRunners+"\nunit.core.interfaces")

