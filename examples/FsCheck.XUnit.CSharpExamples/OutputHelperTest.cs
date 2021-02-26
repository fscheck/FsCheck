using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using FsCheck;
using FsCheck.Xunit;

using Xunit;
using Xunit.Abstractions;

namespace FsCheck.XUnit.CSharpExamples
{
    public class OutputhelperTest
    {
        private readonly ITestOutputHelper _outputHelper;

        public OutputhelperTest(ITestOutputHelper outputHelper)
        {
            _outputHelper = outputHelper;
        }

        [Property]
        public Property ShouldFail_WritesTextToOutputHelper(int a, int b)
        {
            int real = Calculate(a, b);
            int expected = a + b;

            if (real != expected)
            {
                _outputHelper.WriteLine($"Failed! {real} != {expected}, given {a} and {b}");
            }

            return (real == expected).ToProperty();
        }

        [Property]
        public Property ShouldFail_WritesLabelToOutputHelper(int a, int b)
        {
            int real = Calculate(a, b);
            int expected = a + b;

            

            return (real == expected).Label($"Failed! {real} != {expected}, given {a} and {b}");
        }



        public int Calculate(int a, int b)
        {
            return a + b + 1;
        }
    }
}

