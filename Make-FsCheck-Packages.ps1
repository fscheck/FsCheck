nuget pack -sym FsCheck\FsCheck.fsproj -Prop Configuration=Release
nuget pack -sym FsCheck.Xunit\FsCheck.Xunit.fsproj -Prop Configuration=Release
nuget pack -sym FsCheck.NUnit\FsCheck.NUnit.fsproj -Prop Configuration=Release
nuget pack -sym FsCheck.MsTest\FsCheck.MsTest.fsproj -Prop Configuration=Release