using Xunit;
using Xunit.Abstractions;

namespace FsCheck.Test.CSharp
{
    /// <summary>
    /// Reproduction test for GitHub issue: Lifetime problem with Xunit: InvalidOperationException: There is no currently active test.
    /// This test class verifies that mixing Property and Fact tests with ITestOutputHelper doesn't cause lifetime issues.
    /// </summary>
    public class TestOutputHelperLifetimeTests
    {
        private readonly ITestOutputHelper _testOutputHelper;

        public TestOutputHelperLifetimeTests(ITestOutputHelper testOutputHelper)
        {
            _testOutputHelper = testOutputHelper;
        }

        [FsCheck.Xunit.Property]
        public void Test1()
        {
            _testOutputHelper.WriteLine($"{nameof(Test1)}");
        }

        [Fact]
        public void Test2()
        {
            _testOutputHelper.WriteLine($"{nameof(Test2)}");
        }

        [FsCheck.Xunit.Property]
        public void Test3()
        {
            _testOutputHelper.WriteLine($"{nameof(Test3)}");
        }

        [Fact]
        public void Test4()
        {
            _testOutputHelper.WriteLine($"{nameof(Test4)}");
        }
    }
}
