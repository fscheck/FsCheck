cls

.nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
.nuget\nuget.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
.nuget\nuget.exe install FSharp.Formatting -OutputDirectory packages -ExcludeVersion

packages\FAKE\tools\FAKE.exe build.fsx $args
