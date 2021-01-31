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
        private Config MyQuick { get; set; }
        private Config MyVerbose { get; set; }

        [TestInitialize]
        public void Initialize()
        {
            MyQuick = Config.QuickThrowOnFailure.WithArbitrary(new[] { typeof(MyArbitraries) } );          
            MyVerbose = Config.VerboseThrowOnFailure.WithArbitrary(new[] { typeof(MyArbitraries) });
        }
        
        [TestMethod]
        public void VerboseTest()
        {
            Prop.ForAll<Doc>(doc => doc.ToString() != "")
                .Check(MyVerbose);
        }

        [TestMethod]
        public void QuickTest()
        {
            Prop.ForAll<Doc>(doc => doc.ToString() != "")
                .Check(MyQuick);
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
