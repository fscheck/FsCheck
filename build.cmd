@echo off

cls
dotnet --info
dotnet tool restore
dotnet fsi build.fsx %*
