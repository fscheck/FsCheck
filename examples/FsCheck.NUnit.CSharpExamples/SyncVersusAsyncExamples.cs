using System.Threading.Tasks;
using FsCheck.Fluent;
using NUnit.Framework;

namespace FsCheck.NUnit.CSharpExamples;

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
        Assert.That(b ^ !b);
    }

    [Property]
    public async Task Task_Exception_ShouldFail(bool b)
    {
        await DoSomethingAsync();
        Assert.That(b && !b);
    }

    [Property]
    public async Task Task_Cancelled_ShouldFail(bool b)
    {
        await Task.Run(() => Assert.That(b ^ !b), new System.Threading.CancellationToken(canceled: true));
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
