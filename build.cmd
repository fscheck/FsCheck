@echo off

cls
dotnet --info
dotnet tool restore
dotnet fake run build.fsx %*
