using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class DocTest
    {
        [TestInitialize]
        public void Initialize()
        {
            Arb.Register<MyArbitraries>();          
        }
        
        [TestMethod]
        public void VerboseTest()
        {
            Prop.ForAny<Doc>(doc => doc.ToString() != "")
                .AssertVerbose(); 
        }

        [TestMethod]
        public void QuickTest()
        {
            Prop.ForAny<Doc>(doc => doc.ToString() != "")
                .Assert();
        }

        private class MyArbitraries
        {
            public static Arbitrary<Doc> Doc()
            {
                return Gen.Sized(DocGenenerator.Generator).ToArbitrary();
            }            
        }        
    }
}
