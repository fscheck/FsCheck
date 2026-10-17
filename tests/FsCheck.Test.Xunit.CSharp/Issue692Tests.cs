using FsCheck.Xunit;
using Xunit;
using Xunit.Abstractions;

public class Issue692Tests(ITestOutputHelper testOutputHelper)
{
    [Property(MaxTest = 1)]
    public bool Test1()
    {
        testOutputHelper.WriteLine(nameof(Test1));
        return true;
    }

    [Fact]
    public void Test2()
    {
        testOutputHelper.WriteLine(nameof(Test2));
    }
}
