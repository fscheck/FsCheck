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
            Arb.Register<MyArbitraries>();          
        }
        
        [TestMethod]
        public void VerboseTest()
        {
            Prop.ForAll<Doc>(doc => doc.ToString() != "")
                .VerboseCheckThrowOnFailure(); 
        }

        [TestMethod]
        public void QuickTest()
        {
            Prop.ForAll<Doc>(doc => doc.ToString() != "")
                .QuickCheckThrowOnFailure();
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
