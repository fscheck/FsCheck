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
        public void Test1(int x)
        {
            _testOutputHelper.WriteLine($"{nameof(Test1)}: {x}");
        }

        [Fact]
        public void Test2()
        {
            _testOutputHelper.WriteLine($"{nameof(Test2)}");
        }

        [FsCheck.Xunit.Property]
        public void Test3(string s)
        {
            _testOutputHelper.WriteLine($"{nameof(Test3)}: {s ?? "null"}");
        }

        [Fact]
        public void Test4()
        {
            _testOutputHelper.WriteLine($"{nameof(Test4)}");
        }

        /// <summary>
        /// This test specifically exercises the Every and EveryShrink callbacks by enabling Verbose mode.
        /// These callbacks capture the TestOutputHelper in closures, which was the root cause of the lifetime issue.
        /// </summary>
        [FsCheck.Xunit.Property(Verbose = true, MaxTest = 5)]
        public bool Test5_VerboseMode(int x, int y)
        {
            _testOutputHelper.WriteLine($"{nameof(Test5_VerboseMode)}: x={x}, y={y}");
            return true; // Always pass
        }
    }
}
