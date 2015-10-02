param($installPath, $toolsPath, $package, $project)

write-host "Adding a reference to nunit.core and nunit.core.interfaces to the project"
$packagesFolder = Split-Path -Path $installPath -Parent
$version = ""

# Get-Package -Filter NUnit.Runners also returns packages with NUnit.Runners in the description (e.g. NUnit or SpecFlow.NUnit.Runners)
# we need to find the one, that we are looking for
$nunitPackages = Get-Package -Filter NUnit.Runners
foreach ($nunitPackage in $nunitPackages)
{
	# there is a bug in NuGet: the package id for all returned packages is "NUnit.Runners", so filtering that is not enough
	if ($nunitPackage.Id = "NUnit.Runners" -and $nunitPackage.ProjectUrl.ToString().Contains("nunit")) 
	{ 
		$version = $nunitPackage.Version.ToString() 
	}
}

if ($version -ne "") 
{ 
	$nunitPackageToolsLibFolder = $packagesFolder + "\NUnit.Runners." + $version + "\tools\lib\"
    
    write-host "The nunit location is: " + $nunitPackageToolsLibFolder
    
	$nunitCoreInterfacesRef = $project.Object.References.Item("nunit.core.interfaces")
	if ($nunitCoreInterfacesRef) { $nunitCoreInterfacesRef.Remove() }
	$project.Object.References.Add($nunitPackageToolsLibFolder + "nunit.core.interfaces.dll")
    
    $nunitCoreRef = $project.Object.References.Item("nunit.core")
	if ($nunitCoreRef) { $nunitCoreRef.Remove() }
	$project.Object.References.Add($nunitPackageToolsLibFolder + "nunit.core.dll")
}


