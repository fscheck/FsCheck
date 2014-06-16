@echo off

cls

.nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
.nuget\nuget.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
.nuget\nuget.exe install FSharp.Formatting -OutputDirectory packages -ExcludeVersion

SET TARGET="All"

IF NOT [%1]==[] (set TARGET="%1")

"packages\FAKE\tools\FAKE.exe" "build.fsx" "target=%TARGET%"