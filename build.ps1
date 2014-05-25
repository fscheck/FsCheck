cls
If (-Not (Test-Path packages\FAKE\tools\Fake.exe)) {
  .nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
}
If (-Not (Test-Path packages\SourceLink.Fake\tools\SourceLink.fsx)) {
  .nuget\nuget.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
}
packages\FAKE\tools\FAKE.exe build.fsx $args
