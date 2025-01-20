using System.Threading.Tasks;
using FsCheck.Fluent;
using FsCheck.Xunit;
using Xunit;

namespace FsCheck.XUnit.CSharpExamples;

public class SyncVersusAsyncExamples
{
    [Property]
    public Property Property_ShouldPass(bool b)
    {
        return (b ^ !b).Label("b ^ !b");
    }

    [Property]
    public Property Property_ShouldFail(bool b)
    {
        return (b && !b).Label("b && !b");
    }

    [Property]
    public async Task Task_ShouldPass(bool b)
    {
        await DoSomethingAsync();
        Assert.True(b ^ !b);
    }

    [Property]
    public async Task Task_Exception_ShouldFail(bool b)
    {
        await DoSomethingAsync();
        Assert.True(b && !b);
    }

    [Property]
    public async Task Task_Cancelled_ShouldFail(bool b)
    {
        await Task.Run(() => Assert.True(b ^ !b), new System.Threading.CancellationToken(canceled: true));
    }

    [Property]
    public async Task<Property> TaskProperty_ShouldPass(bool b)
    {
        await DoSomethingAsync();
        return (b ^ !b).Label("b ^ !b");
    }

    [Property]
    public async Task<Property> TaskProperty_ShouldFail(bool b)
    {
        await DoSomethingAsync();
        return (b && !b).Label("b && !b");
    }

    private static async Task DoSomethingAsync() => await Task.Yield();
}
