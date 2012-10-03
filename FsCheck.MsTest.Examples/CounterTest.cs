using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class CounterTest
    {
        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void QuickCounter()
        {
            (new CounterSpec()).Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void VerboseCounter()
        {
            (new CounterSpec()).AssertVerbose();
        }
    }
}
