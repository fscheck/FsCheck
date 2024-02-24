@echo off

cls
dotnet --info
dotnet tool restore
dotnet restore
dotnet fake run build.fsx %*
