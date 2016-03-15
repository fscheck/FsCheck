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
            new CounterSpec().ToProperty().QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void VerboseCounter()
        {
            new CounterSpec().ToProperty().VerboseCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void CheckCounterWithQuick()
        {
            new CounterSpec().ToProperty().Check(Configuration.QuickThrowOnFailure);
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void CheckCounterWithVerbose()
        {
            new CounterSpec().ToProperty().Check(Configuration.VerboseThrowOnFailure);
        }
    }
}
