using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using FsCheck.Fluent;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class DocTest
    {
        [TestInitialize]
        public void Initialize()
        {
            DefaultArbitraries.Add<MyArbitraries>();          
        }
        
        [TestMethod]
        public void VerboseTest()
        {
            Spec.ForAny<Doc>(doc => doc.ToString() != "")
                .AssertVerbose(); 
        }

        [TestMethod]
        public void QuickTest()
        {
            Spec.ForAny<Doc>(doc => doc.ToString() != "")
                .Assert();
        }

        private class MyArbitraries
        {
            public static Arbitrary<Doc> Doc()
            {
                return Any.OfSize(DocGenenerator.Generator).ToArbitrary();
            }            
        }        
    }
}
