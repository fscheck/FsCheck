using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class CounterTest
    {
        [TestMethod, ExpectedException(typeof(Exception))]
        public void QuickCounter()
        {
            new CounterSpec().ToSpecification().QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void VerboseCounter()
        {
            new CounterSpec().ToSpecification().VerboseCheckThrowOnFailure();
        }
    }
}
